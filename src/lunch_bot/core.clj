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
     [rtm-transmit :as tx]]))


(def api-token-filename "abot-api-token.txt")

(def money-events-filename "events.txt")

(def ^:private money-events (atom nil))

(defn initialize-money-events []
  (swap! money-events (fn [_] (store/read-events money-events-filename))))


(defn get-channel-command-signature []
  "generates a regex that will match against a channel message that should
  be interpreted as a command, and captures the command text from the message."
  (let [linkified-self (tx/linkify (state/self-id))]
    (str linkified-self ":? *(.*)")))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (if (state/dm? channel-id)
    text
    (when-let [[_ cmd-text] (-> (get-channel-command-signature)
                                (re-pattern)
                                (re-find text))]
      cmd-text)))


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
  (store/write-events @money-events)
  (talk/event->str event))


(defmulti handle-slack-event :type)

(defmethod handle-slack-event "message"
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-text (message->command-text channel-id text)]
      (let [cmd (comm/message->command user-id cmd-text)
            cmd-reply (handle-command cmd)]
        (when cmd-reply
          (talk/say-message channel-id cmd-reply))))))

(defmethod handle-slack-event "channel_joined" [event]
  nil)

(defmethod handle-slack-event :default [event]
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
      (slack/connect api-token handle-slack-event)
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

