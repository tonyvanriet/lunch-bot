(ns lunch-bot.core
  (:gen-class)
  (:require [clj-slack-client.core :as slack]))


(def abot-api-token "xoxb-3215140999-UuVgqNVwxMDcWNrVeoOMMtxw")
(def someotherbot-api-token "xoxb-3246812512-FRBtlsTndTc2fGEhwq1rOhcD")
(def tonyvanriet-api-token "xoxp-3215134233-3215134235-3216767432-ca2d3d")


(defn -main
  [& args]

  (slack/connect abot-api-token)

  (println "lunch-bot running...")

  (loop []
    (let [input (read-line)]
      (if (= input "q")
        (do
          (slack/disconnect)
          (shutdown-agents)
          (println "lunch-bot dying"))
        (recur)))))



