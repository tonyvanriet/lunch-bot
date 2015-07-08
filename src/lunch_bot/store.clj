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

(defn- update-date->inst [m]
  "returns the map m with :date fields converted from dates to insts"
  (if (contains? m :date)
    (update-in m [:date] date->inst)
    m))

(defn- update-inst->local-date [m]
  "returns the map m with :date fields converted from insts to LocalDates"
  (if (contains? m :date)
    (update-in m [:date] inst->local-date)
    m))


(defn- edn->maps
  [edn]
  (->> edn
       (edn/read-string)
       (map #(update-inst->local-date %))
       (into [])))

(defn- maps->edn
  [maps]
  (->> maps
       (map #(update-date->inst %))
       (pr-str)))


(defn read-maps
  [filename]
  (->> filename
       (slurp-filename)
       (edn->maps)))

(defn read-events [filename] (read-maps filename))
(defn read-restaurants [filename] (read-maps filename))


(defn overwrite-events
  [events filename]
  (spit filename (maps->edn events)))

(defn append-event
  [event filename]
  (spit filename (maps->edn [event]) :append true))

(defn append-command
  [cmd filename]
  (when (not= :find-nags (:command-type cmd))
    (spit filename (maps->edn [cmd]) :append true)))

