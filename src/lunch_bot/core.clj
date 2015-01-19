(ns lunch-bot.core
  (:gen-class)
  (:require
   [clj-slack-client
    [core :as slack]
    [team-state :as state]
    [rtm-transmit :as tx]]))


(def abot-api-token "xoxb-3215140999-UuVgqNVwxMDcWNrVeoOMMtxw")
(def someotherbot-api-token "xoxb-3246812512-FRBtlsTndTc2fGEhwq1rOhcD")
(def tonyvanriet-api-token "xoxp-3215134233-3215134235-3216767432-ca2d3d")


(defmulti handle-event :type)

(defmethod handle-event "message"
  [event]
  (let [user-id (:user event)
        user (state/get-user user-id)
        self-id (:id (state/get-self))
        channel-id (:channel event)]
    (when (and (not= user-id self-id)
               (not (:is_bot user)))
      (tx/say-message channel-id "That's what she said"))))

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
  (shutdown-agents)
  (println "...lunch-bot dying"))


(defn -main
  [& args]

  (try

    (slack/connect abot-api-token handle-event)
    (println "lunch-bot running...")

    (wait-for-console-quit)

    (finally (shutdown-app))))



