(ns lunch-bot.store
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.string :as str]))


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


(defn- edn->map
  [edn]
  (-> edn
      (edn/read-string)
      (update-inst->local-date)))

(defn- edn->maps
  "converts each line of the edn string into a map and returns the resulting collection of maps"
  [edn]
  (when edn
    (->> edn
         (str/split-lines)
         (map edn->map)
         (into []))))


(defn map->edn
  "returns a string with the map m converted to edn"
  [m]
  (-> m
      (update-date->inst)
      (pr-str)))

(defn- maps->edn
  "returns a string with a line for each map in ms, converted to edn"
  [ms]
  (let [ms-edn (map map->edn ms)
        ms-edn-lines (interleave ms-edn (repeat "\n"))]
    (reduce str ms-edn-lines)))


(defn read-maps
  [filename]
  (->> filename
       (slurp-filename)
       (edn->maps)))

(defn read-events [filename] (read-maps filename))
(defn read-restaurants [filename] (read-maps filename))


(defn overwrite-events
  [events filename]
  (let [events-edn (maps->edn events)]
    (spit filename events-edn)))

(defn append-event
  [event filename]
  (spit filename (str (map->edn event) "\n") :append true))

(defn append-command
  [cmd filename]
  (when (not= :find-nags (:command-type cmd))
    (spit filename (str (map->edn cmd) "\n") :append true)))

