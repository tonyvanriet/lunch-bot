(ns lunch-bot.core
  (:gen-class)
  (:require
   [clj-slack-client
    [core :as slack]
    [team-state :as state]
    [rtm-transmit :as tx]]))


(def api-token (->> "api-token.txt"
                    (slurp)
                    (clojure.string/trim)))


(defn handle-command
  [channel-id event]
  (tx/say-message channel-id (:text event)))


(defmulti handle-event :type)

(defmethod handle-event "message"
  [event]
  (let [user-id (:user event)
        user (state/get-user user-id)
        self-id (:id (state/get-self))
        channel-id (:channel event)]
    (when (and (not= user-id self-id)
               (not (:is_bot user))
               (re-matches #"lunch .*" (:text event)))
      (handle-command channel-id event))))

(defmethod handle-event "channel_joined"
  [event]
  nil)

(defmethod handle-event :default
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


(defn start []
  (slack/connect api-token handle-event)
  (println "lunch-bot running..."))

(defn stop []
  (shutdown-app))


(defn -main
  [& args]

  (try
    (start)
    (wait-for-console-quit)
    (finally
     (stop)
     (shutdown-agents))))

