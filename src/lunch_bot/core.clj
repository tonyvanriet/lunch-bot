(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot
     [command :as command]
     [money :as money]
     [talk :as talk]
     [store :as store]
     [meal :as meal]]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [web :as web]]
    [clj-time.core :as time]))


(def api-token-filename "api-token.txt")

(def ^:dynamic *api-token* nil)


(def money-events-filename "money-events.edn")

(def ^:private money-events (atom []))

(defn initialize-money-events []
  (swap! money-events (fn [_] (into [] (store/read-events money-events-filename)))))


(def meal-events-filename "meal-events.edn")

(def ^:private meal-events (atom []))

(defn initialize-meal-events []
  (swap! meal-events (fn [_] (into [] (store/read-events meal-events-filename)))))


(defn get-lunch-channel [] (state/name->channel "lunch"))


(defmulti handle-command :command-type)

(defmethod handle-command :unrecognized
  [_]
  "huh?")

(defmethod handle-command :show
  [{:keys [info-type requestor]}]
  (case info-type
    :balances (->> @money-events
                   (money/events->balances)
                   (money/sort-balances)
                   (reverse)
                   (talk/balances->str))
    :pay? (->> @money-events
               (money/events->balances)
               (money/best-payment requestor)
               (talk/event->str))
    :payoffs (->> @money-events
                  (money/events->balances)
                  (money/minimal-payoffs)
                  (talk/payoffs->str))
    :history (talk/events->str @money-events)
    :today (->> (concat @meal-events @money-events)
                (filter #(= (:date %) (time/today)))
                (meal/events->meal)
                (talk/today-summary))))

(defmethod handle-command :event
  [{:keys [event]}]
  (swap! money-events (fn [events] (conj events event)))
  (store/write-events @money-events money-events-filename)
  (talk/event->reply-str event))

(defmethod handle-command :meal-event
  [{:keys [meal-event]}]
  (swap! meal-events (fn [events] (conj events meal-event)))
  (store/write-events @meal-events meal-events-filename)
  (when (= (:type meal-event) :choose)
    (let [restaurant (:restaurant meal-event)
          channel-id (:id (get-lunch-channel))]
      (web/channels-setTopic *api-token* channel-id
                             (str "ordering " (:name restaurant)))))
  (talk/event->reply-str meal-event))


(defmulti handle-slack-event (juxt :type :subtype))

(defmethod handle-slack-event ["message" nil]
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-text (command/message->command-text channel-id text)]
      (let [cmd (command/message->command user-id cmd-text)
            cmd-reply (handle-command cmd)]
        (when cmd-reply
          (talk/say-message channel-id cmd-reply))))))

(defmethod handle-slack-event ["message" "message_changed"]
  [{channel-id :channel, {user-id :user, text :text} :message}]
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
      (initialize-money-events)
      (initialize-meal-events)
      (alter-var-root (var *api-token*) (fn [_] api-token))
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


; todo 'order’ or ‘order?’ without food shows restaurant info and numbered order history
; todo ‘order usual’ - usual defaults to most recent order, or user setting
; todo 'usual food food food' - set usual for chosen restaurant
; todo 'today shows aggregate summary of the day's meal events
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
