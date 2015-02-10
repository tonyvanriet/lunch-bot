(ns lunch-bot.store
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]))


(defn read-api-token
  [filename]
  (let [file (io/file filename)]
    (if (.exists file)
      (string/trim (slurp file))
      (do (spit filename "your-api-token")
          (println "put your api token in" filename)))))


(defn- date->inst [date] (when date (.toDate date)))

(defn- inst->local-date [inst] (when inst (org.joda.time.LocalDate. inst)))
(defn- inst->date-time [inst] (when inst (org.joda.time.DateTime. inst)))


(defn- edn->events
  [edn]
  (->> edn
       (edn/read-string)
       (map #(update-in % [:date] inst->local-date))
       (into [])))


(defn- events->edn
  [events]
  (->> events
       (map #(update-in % [:date] date->inst))
       (pr-str)))


(defn read-events
  [filename]
  (let [file (io/file filename)]
    (when (and (.exists file)
               (< 0 (.length file)))
      (->> file
           (slurp)
           (edn->events)))))


(defn write-events
  [events filename]
  (spit filename (events->edn events)))



