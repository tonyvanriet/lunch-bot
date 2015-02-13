(ns lunch-bot.talk
  (:require [clj-slack-client.rtm-transmit :as tx]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as time]
            [clj-slack-client.team-state :as ts]))


(defn pstr
  "pretty string"
  [object]
  (with-out-str (pprint object)))


(defn person->str [person] (ts/id->name person))


(defn balances->str [balances]
  (pstr (map #(str (key %) " " (val %)) balances)))

(defn payoffs->str
  [payoffs]
  (pstr (map (fn [{:keys [person amount to]}]
               (str (person->str person)
                    " pays "
                    (person->str to) " "
                    amount))
             payoffs)))


(defmulti event->str :type)

(defmethod event->str :paid [{:keys [person amount to date]}]
  (str (person->str person) " paid " (person->str to) " " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :bought [{:keys [person amount date]}]
  (str (person->str person) " bought lunch for " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :cost [{:keys [person amount date]}]
  (str (person->str person) "'s lunch cost " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :choose [{:keys [restaurant]}]
  (str "chose " (:name restaurant)))

(defmethod event->str :in [{:keys [person]}]
  (str (person->str person) "'s in"))

(defmethod event->str :out [{:keys [person]}]
  (str (person->str person) "'s out"))

(defmethod event->str :order [{:keys [person food]}]
  (str person " wants " food))


(defn events->str [events]
  (pstr (map #(event->str %) events)))


(defn event->reply-str
  [event]
  (case (:type event)
    (:paid :bought :cost) (event->str event)
    :choose nil
    :in (str (rand-nth [":metal:" ":rocket:" ":clap:" ":thumbsup:" ":dancers:"]))
    :out (str (rand-nth [":fu:" ":fire:" ":facepunch:" ":thumbsdown:" ":hankey:"]))
    :order (str "mmm")))



(defn say-message
  [channel-id message]
  (tx/say-message channel-id message))

