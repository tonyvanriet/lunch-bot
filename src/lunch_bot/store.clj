(ns lunch-bot.store
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]))


(defn slurp-filename
  [filename]
  (let [file (io/file filename)]
    (when (and (.exists file)
               (< 0 (.length file)))
      (slurp file))))


(defn read-api-token
  [filename]
  (if-let [raw-api-token (slurp-filename filename)]
    (string/trim raw-api-token)
    (do (spit filename "your-api-token")
        (println "put your api token in" filename))))


(defn- date->inst [date] (when date (.toDate date)))

(defn- inst->local-date [inst] (when inst (org.joda.time.LocalDate. inst)))
(defn- inst->date-time [inst] (when inst (org.joda.time.DateTime. inst)))


(defn- edn->events
  [edn]
  (->> edn
       (edn/read-string)
       (map #(update-in % [:date] inst->local-date))
       (into [])))

(defn- edn->restaurants
  [edn]
  (->> edn
       (edn/read-string)
       (into [])))


(defn- events->edn
  [events]
  (->> events
       (map #(update-in % [:date] date->inst))
       (pr-str)))


(defn read-events
  [filename]
  (->> filename
       (slurp-filename)
       (edn->events)))


(defn overwrite-events
  [events filename]
  (spit filename (events->edn events)))

(defn append-event
  [event filename]
  (spit filename (events->edn [event]) :append true))


(defn read-restaurants
  [filename]
  (->> filename
       (slurp-filename)
       (edn->restaurants)))


