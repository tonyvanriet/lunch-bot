(ns lunch-bot.event
  (:require [lunch-bot.store :as store]))


(def events-filename "events.edn")


(def committed-events (atom []))


(defn initialize-events []
  (swap! committed-events (constantly (into [] (store/read-events events-filename)))))


(defn commit-event
  [event]
  (swap! committed-events #(conj % event))
  (store/write-events @committed-events events-filename))
