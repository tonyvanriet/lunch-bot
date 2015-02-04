(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot.comm :as comm]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [rtm-transmit :as tx]]))


(defn api-token []
  (->> "api-token.txt"
       (slurp)
       (clojure.string/trim)))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (if (state/dm? channel-id)
    text
    (let [linkified-self (tx/linkify (state/self-id))]
      (when-let [[_ cmd-text] (re-find (re-pattern (str linkified-self ":? *(.*)")) text)]
        cmd-text))))


(defmulti handle-command :command-type)

(defmethod handle-command :unrecognized
  [_]
  "huh?")

(defmethod handle-command :show
  [{:keys [info-type]}]
  (str "showing " info-type))

(defmethod handle-command :event
  [{:keys [event]}]
  (str "adding event " event))


(defmulti handle-slack-event :type)

(defmethod handle-slack-event "message"
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-text (message->command-text channel-id text)]
      (let [cmd (comm/message->command user-id cmd-text)
            cmd-reply (handle-command cmd)]
        (when cmd-reply
          (tx/say-message channel-id cmd-reply))))))

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


(defn start []
  (slack/connect api-token handle-slack-event)
  (println "lunch-bot running..."))

(defn stop []
  (shutdown-app))

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

