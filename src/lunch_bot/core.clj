(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot.comm :as comm]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]]))


(def api-token (->> "api-token.txt"
                    (slurp)
                    (clojure.string/trim)))


(defmulti handle-event :type)

(defmethod handle-event "message" [event]
  (comm/handle-message event))

(defmethod handle-event "channel_joined" [event]
  nil)

(defmethod handle-event :default [event]
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

