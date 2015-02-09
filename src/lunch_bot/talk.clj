(ns lunch-bot.talk
  (:require [clj-slack-client.rtm-transmit :as tx]
            [clojure.pprint :refer [pprint]]))


(defn pstr
  "pretty string"
  [object]
  (with-out-str (pprint object)))


(defn balances->str [balances]
  (pstr (map #(str (key %) " " (val %)) balances)))

(defn payoffs->str
  [payoffs]
  (pstr (map (fn [{:keys [person amount to]}]
               (str person " pays " to " " amount))
             payoffs)))

(defmulti event->str :type)
(defmethod event->str :paid [{:keys [person amount to date]}]
  (str person " paid " to " " amount " on " date))
(defmethod event->str :bought [{:keys [person amount date]}]
  (str person " bought lunch for " amount " on " date))
(defmethod event->str :cost [{:keys [person amount date]}]
  (str person "'s lunch cost " amount " on " date))

(defn events->str [events]
  (pstr (map #(event->str %) events)))


(defn say-message
  [channel-id message]
  (tx/say-message channel-id message))

