(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot
     [command :as comm]
     [money :as money]
     [talk :as talk]
     [store :as store]]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [rtm-transmit :as tx]
     [web :as web]]))


(def api-token-filename "api-token.txt")

(def ^:dynamic *api-token* nil)


(def money-events-filename "money-events.edn")

(def ^:private money-events (atom []))

(defn initialize-money-events []
  (swap! money-events (fn [_] (into [] (store/read-events money-events-filename)))))


(def ^:private meal-events (atom []))


(defn get-lunch-channel [] (state/name->channel "lunch"))


(defn get-channel-command-signature []
  "generates a regex that will match against a channel message that should
  be interpreted as a command, and captures the command text from the message."
  (let [linkified-self (tx/linkify (state/self-id))]
    (str linkified-self ":? *(.*)")))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (when text
    (if (state/dm? channel-id)
      text
      (when-let [[_ cmd-text] (-> (get-channel-command-signature)
                                  (re-pattern)
                                  (re-find text))]
        cmd-text))))


(defmulti handle-command :command-type)

(defmethod handle-command :unrecognized
  [_]
  "huh?")

(defmethod handle-command :show
  [{:keys [info-type]}]
  (case info-type
    :balances (->> @money-events
                   (money/events->balances)
                   (talk/balances->str))
    :payoffs (->> @money-events
                  (money/events->balances)
                  (money/minimal-payoffs)
                  (talk/payoffs->str))
    :events (talk/events->str @money-events)))

(defmethod handle-command :event
  [{:keys [event]}]
  (swap! money-events (fn [events] (conj events event)))
  (store/write-events @money-events money-events-filename)
  (talk/event->str event))

(defmethod handle-command :meal-event
  [{:keys [meal-event]}]
  (swap! meal-events (fn [events] (conj events meal-event)))
  (when (= (:type meal-event) :choose)
    (let [restaurant (:restaurant meal-event)
          channel-id (:id (get-lunch-channel))]
      (web/channels-setTopic *api-token* channel-id
                             (str "ordering " (:name restaurant)))))
  (talk/meal-event->str meal-event))


(defmulti handle-slack-event #(vector (:type %) (:subtype %)))

(defmethod handle-slack-event ["message" nil]
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-text (message->command-text channel-id text)]
      (let [cmd (comm/message->command user-id cmd-text)
            cmd-reply (handle-command cmd)]
        (when cmd-reply
          (talk/say-message channel-id cmd-reply))))))

(defmethod handle-slack-event ["message" "message_changed"]
  [{channel-id :channel, {user-id :user, text :text} :message}]
  (when (message->command-text channel-id text)
    (talk/say-message channel-id "I can't handle edited messages... yet")))

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

