(ns lunch-bot.talk
  (:require [clj-slack-client.rtm-transmit :as tx]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as time]
            [clj-slack-client.team-state :as ts]
            [clojure.string :as str]))


(defn str-coll
  [object]
  (if (coll? object)
    (->> (map str object)
         (interpose "\n")
         (apply str))
    (str object)))


(defn person->str [person] (ts/id->name person))


(defn balances->str [balances]
  (str-coll (map #(str (person->str (key %)) " " (val %)) balances)))

(defn payoffs->str
  [payoffs]
  (str-coll (map (fn [{:keys [person amount to]}]
                   (str (person->str person)
                        " pays "
                        (person->str to) " "
                        amount))
                 payoffs)))


(defmulti event->str :type)

(defmethod event->str :bought [{:keys [person amount date]}]
  (str (person->str person) " bought lunch for " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :cost [{:keys [person amount date]}]
  (str (person->str person) "'s lunch cost " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :paid [{:keys [person amount to date]}]
  (str (person->str person) " paid " (person->str to) " " amount
       (when (not= date (time/today)) (str " on " date))))

(defmethod event->str :should-pay [{:keys [person amount to]}]
  (str (person->str person) " should pay " (person->str to) " " amount))

(defmethod event->str :choose [{:keys [restaurant]}]
  (str "chose " (:name restaurant)))

(defmethod event->str :in [{:keys [person]}]
  (str (person->str person) "'s in"))

(defmethod event->str :out [{:keys [person]}]
  (str (person->str person) "'s out"))

(defmethod event->str :order [{:keys [person food]}]
  (str (person->str person) " wants " food))


(defn events->str [events]
  (str-coll (map event->str events)))


(defn recent-money-history [events]
  (let [min-date (time/minus (time/today) (time/days 7))
        recent-events (filter #(time/after? (:date %) min-date) events)]
    (if (seq recent-events)
      (str "History as of " min-date "\n"
           (events->str recent-events))
      (str "No history as of " min-date))))


(defn event->reply-str
  [event]
  (case (:type event)
    (:paid :bought :cost :should-pay) (event->str event)
    :choose nil
    :in (str (rand-nth [":metal:" ":rocket:" ":clap:" ":thumbsup:" ":dancers:"]))
    :out (str (rand-nth [":fu:" ":fire:" ":facepunch:" ":thumbsdown:" ":hankey:"]))
    :order (str "mmm")))


(defn meal->orders-str
  [meal]
  (let [people-with-orders (filter #(contains? (val %) :order) (:people meal))]
    (for [person-with-order people-with-orders
          :let [person (key person-with-order)
                order (:order (val person-with-order))
                order-lines (str/split-lines order)]]
      (str (person->str person) "\n"
           (->> order-lines
                (map #(str ">" % "\n"))
                (apply str))))))


(defn pre-order-summary
  [meal]
  (let [chosen-restaurant-name (-> meal :chosen-restaurant :name)]
    (str (when chosen-restaurant-name (str "Ordering " chosen-restaurant-name "\n"))
         "\n"
         (apply str (meal->orders-str meal)))))

(defn post-order-summary
  [meal]
  (str meal))

(defn today-summary
  [meal]
  (pre-order-summary meal))


(defn say-message
  [channel-id message]
  (tx/say-message channel-id message))

