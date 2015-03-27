(ns lunch-bot.talk
  (:require [clj-slack-client.rtm-transmit :as tx]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as time]
            [clj-slack-client.team-state :as ts]
            [clojure.string :as str]
            [lunch-bot.meal :as meal]))


(defn str-coll
  [object]
  (if (coll? object)
    (->> (map str object)
         (interpose "\n")
         (apply str))
    (str object)))


(defn person->str [person]
  (if-let [name (ts/id->name person)]
    name
    person))

(defn people->str
  "returns a comma-delimited str of people"
  [people]
  (let [people-strs (map person->str people)]
    (case (count people-strs)
      0 nil
      1 (first people-strs)
      2 (str (first people-strs) " and " (second people-strs))
      (str (apply str (interpose ", " (butlast people-strs)))
           ", and " (last people-strs)))))


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


(defn dispatch-event->str [event] (:type event))

(defmulti event->str #'dispatch-event->str)

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
    (apply str (for [person-with-order people-with-orders
                     :let [person (key person-with-order)
                           order (:order (val person-with-order))]]
                 (str (person->str person) "\n"
                      "```" order "```" "\n")))))


(defn pre-order-summary
  [meal]
  (let [chosen-restaurant-name (-> meal :chosen-restaurant :name)
        ins (meal/people-in meal)
        outs (meal/people-out meal)
        orderless-ins (filter #(not (meal/person-ordered? meal %)) ins)]
    (str (if chosen-restaurant-name
           (str "Ordering " chosen-restaurant-name "\n")
           (str "Waiting for somebody to choose a restaurant" "\n"))
         (when (seq ins)
           (str (people->str ins) " " (if (= (count ins) 1) "is" "are") " *in*" "\n"))
         (when (seq outs)
           (str (people->str outs) " " (if (= (count outs) 1) "is" "are") " *out*" "\n"))
         (when (seq orderless-ins)
           (str "Waiting for " (if (= (count orderless-ins) 1) "an order" "orders")
                " from " (people->str orderless-ins) "\n"))
         (meal->orders-str meal))))

(defn post-order-summary
  [meal]
  (let [chosen-restaurant-name (-> meal :chosen-restaurant :name)
        ins (meal/people-in meal)
        multiple-ins? (> (count ins) 1)
        buyers (meal/people-bought meal)
        buyers-str (people->str buyers)
        multiple-buyers? (> (count buyers) 1)
        costless-ins (filter #(not (meal/person-costed? meal %)) ins)
        buyer-surplus (- (meal/total-bought meal) (meal/total-cost meal))]
    (if chosen-restaurant-name
      (str "Ordered from " chosen-restaurant-name "\n"
           (when (seq buyers)
             (str buyers-str " *bought*" "\n"))
           (when (seq ins)
             (str (people->str ins) " " (if multiple-ins? "were" "was") " *in*" "\n"))
           (if (seq costless-ins)
             (str "Waiting for the *cost* of " (people->str costless-ins) "'s lunch"
                  (if (= (count costless-ins) 1) "." "es.") "\n")
             (str buyers-str " " (if multiple-buyers? "are" "is")
                  (cond (= buyer-surplus 0M) (str " squared away")
                        (< buyer-surplus 0M) (str " " (- buyer-surplus) " ahead")
                        (> buyer-surplus 0M) (str " " buyer-surplus " short"))
                  "\n")))
      "No restaurant chosen")))


(defn person-meal->str
  [{:keys [order cost] :as meal-order}]
  (str "```" "order\n"
       order "\n"
       (when cost (str "cost " cost))
       "```\n"))

(defn person-meal-history
  [person-meals restaurant]
  (apply str "Your latest orders for " (:name restaurant) "\n"
         (map person-meal->str person-meals)))


(defn say-message
  [channel-id message]
  (tx/say-message channel-id message))

