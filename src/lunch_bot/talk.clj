(ns lunch-bot.talk
  (:require [clj-slack-client.rtm-transmit :as tx]
            [clojure.pprint :refer [pprint]]))


(defn pstr
  "pretty string"
  [object]
  (with-out-str (pprint object)))


(defn balances->str [balances]
  (pstr (map (fn [bal]
               (str (key bal) " " (val bal)))
             balances)))

(defn payoffs->str
  [payoffs]
  (pstr (map (fn [{:keys [person amount to]}]
               (str person " pays " to " " amount))
             payoffs)))

(defmulti event->str :type)
(defmethod event->str :paid [{:keys [person amount to on]}]
  (str person " paid " to " " amount " on " on))
(defmethod event->str :bought [{:keys [person amount on]}]
  (str person " bought lunch for " amount " on " on))
(defmethod event->str :cost [{:keys [person amount on]}]
  (str person "'s lunch cost " amount " on " on))

(defn events->str [events]
  (pstr (map #(event->str %) events)))


(defn say-message
  [channel-id message]
  (tx/say-message channel-id message))

