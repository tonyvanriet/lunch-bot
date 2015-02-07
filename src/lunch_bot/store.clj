(ns lunch-bot.store
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))


(defn read-api-token
  [filename]
  (let [file (io/file filename)]
    (if (.exists file)
      (string/trim (slurp file))
      (do (spit filename "your-api-token")
          (println "put your api token in" filename)))))



(defn read-events
  [filename]
  (let [file (io/file filename)]
    (when (and (.exists file)
               (< 0 (.length file)))
      (read-string (slurp file)))))



(defn write-events
  [events]
  ;; TODO: write events in edn
  (spit "events.txt" (with-out-str (pprint events))))

