(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot
     [command :as command]
     [money :as money]
     [talk :as talk]
     [store :as store]
     [meal :as meal]
     [event :as event]
     [aggregate :as agg]]
    [lunch-bot.command.handler :as handler]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [web :as web]]
    [clj-time.core :as time]
    [clojure.core.incubator :refer [dissoc-in]])
  (:import (java.math RoundingMode)))

;
; config
;
(def api-token-filename "api-token.txt")
(def lunch-channel-name "lunch")
(def sales-tax-rate 0.0925M)

(def ^:dynamic *api-token* nil)


(defn get-lunch-channel-id [] (:id (state/name->channel lunch-channel-name)))


(defn contextualize-event
  "adds slack message context to event"
  [event {user-id :user, ts :ts, :as msg}]
  (-> event (assoc :person user-id) (assoc :ts ts)))

(defn apply-sales-tax
  "applies the sales-tax-rate to the amount if the events :+tax? is truthy,
   and then removes :+tax?"
  [event]
  (let [taxed-event (if (:+tax? event)
                      (update-in event [:amount] #(-> (* % (+ 1 sales-tax-rate))
                                                      (.setScale 2 RoundingMode/HALF_UP)))
                      event)]
    (dissoc-in taxed-event [:+tax?])))


(defn process-event
  [event]
  ; todo if :out and person has a cost for this meal, create an event to reverse that cost
  (when (= (:type event) :choose)
    (let [restaurant (:restaurant event)
          channel-id (get-lunch-channel-id)]
      (web/channels-setTopic *api-token* channel-id
                             (str "ordering " (:name restaurant))))))


(defn dispatch-handle-command [cmd msg] ((juxt :command-type :info-type) cmd))

(defmulti handle-command
          "performs the computation specified by the command and returns a
          reply string, if any."
          #'dispatch-handle-command)

(defmethod handle-command [:unrecognized nil]
  [_ _]
  "huh?")

(defmethod handle-command [:help nil]
  [_ _]
  (slurp "help.md"))

(defmethod handle-command [:show :balances]
  [_ _]
  (->> (agg/balances)
       (money/sort-balances)
       (reverse)
       (talk/balances->str)))

(defmethod handle-command [:show :pay?]
  [_ {requestor :user}]
  (if-let [payment (money/best-payment requestor (agg/balances))]
    (talk/event->str payment)
    (str "Keep your money.")))

(defmethod handle-command [:show :payoffs]
  [_ _]
  (->> (agg/balances)
       (money/minimal-payoffs)
       (talk/payoffs->str)))

(defmethod handle-command [:show :history]
  [_ _]
  (->> (agg/money-events)
       (talk/recent-money-history)))

(defmethod handle-command [:show :meal-summary]
  [{:keys [date] :as cmd} _]
  (let [meals (agg/meals)
        meal (get meals date)]
    (if (or (time/before? date (time/today)) (meal/any-bought? meal))
      (talk/post-order-summary meal)
      (talk/pre-order-summary meal))))

(defmethod handle-command [:show :ordered?]
  [_ {requestor :user}]
  (let [meals (agg/meals)
        todays-meal (get meals (time/today))]
    (if-let [todays-restaurant (-> todays-meal :chosen-restaurant)]
      (let [person-meals (meal/person-meal-history meals todays-restaurant requestor 3)]
        (talk/person-meal-history person-meals todays-restaurant))
      (str "Somebody needs to choose a restaurant first."))))

(defmethod handle-command [:show :discrepancies]
  [_ _]
  (let [meals (agg/meals)
        discrepant-meals (filter #(meal/is-discrepant (val %)) meals)]
    (talk/discrepant-meals-summary discrepant-meals)))


(defn handle-message
  [{channel-id :channel, text :text, :as msg}]
  (when-let [cmd-text (command/message->command-text channel-id text)]
    ; transform message into command
    (let [cmd (command/command-text->command cmd-text)]
      ; transform command into events
      (if-let [raw-new-events (handler/command->events cmd)]
        (let [new-events (map #(-> %
                                   (contextualize-event msg)
                                   (apply-sales-tax))
                              raw-new-events)
              reply (when (seq new-events)
                      (->> new-events
                           (map talk/event->reply-str)
                           (interpose "\n")
                           (apply str)))]
          (doseq [event new-events]
            (process-event event)
            (event/commit-event event))
          (when reply
            (talk/say-message channel-id reply)))
        (let [reply (handle-command cmd msg)]
          (talk/say-message channel-id reply))))))


(defn dispatch-handle-slack-event [event] ((juxt :type :subtype) event))

(defmulti handle-slack-event #'dispatch-handle-slack-event)

(defmethod handle-slack-event ["message" nil]
  [{user-id :user, :as msg}]
  (when (not (state/bot? user-id))
    (handle-message msg)))

(defmethod handle-slack-event ["message" "message_changed"]
  [{channel-id :channel, {text :text} :message}]
  (when (command/message->command-text channel-id text)
    (talk/say-message channel-id "huh?")))

(defmethod handle-slack-event ["channel_joined" nil]
  [event]
  nil)

(defmethod handle-slack-event :default
  [event]
  nil)


(defn wait-for-console-quit []
  (loop []
    (let [input (read-line)]
      (when-not (= input "q")
        (recur)))))


(defn shutdown-app []
  (slack/disconnect)
  (println "...lunch-bot dying"))


(defn stop []
  (shutdown-app))

(defn start
  ([]
   (start (store/read-api-token api-token-filename)))
  ([api-token]
   (try
     (event/initialize-events)
     (alter-var-root (var *api-token*) (constantly api-token))
     (slack/connect *api-token* handle-slack-event)
     (prn "lunch-bot running...")
     (catch Exception ex
       (println ex)
       (println "couldn't start lunch-bot")
       (stop)))))

(defn restart []
  (stop)
  (start))


(defn -main
  [& args]
  (try
    (start)
    (wait-for-console-quit)
    (finally
      (stop)
      (shutdown-agents))))


; todo ‘order usual’ - usual defaults to most recent order, or user setting
; todo 'usual food food food' - set usual for chosen restaurant
; todo 'yesterday', 'monday', 'last thursday', 'this week', '1/28'
; todo 'add superdawg' or 'restaurant superdawg' - create restaurant
; todo 'add superdawg http://www.superdawg.com/menu.cfm 773-763-0660'
; todo 'superdawg 7737630660 http://www.superdawg.com/menu.cfm' - update restaurant
; todo 'bw3 thursday'
; todo restaurant name links to url
; todo 'remove superdog', 'rename superdog superdawg'
; todo 'payoffs' suggests payments that bring everyone to the average balance, handle balances that don't sum to 0
; todo recognize 'steve paid carla 23' for privileged users
; todo talk converts person's name to "you" in DMs
; todo attempt to interpret multi-line messages as one command per line
; todo lunchbot suggests a restaurant based on history for day of week, or at random
; todo handle edited messages
