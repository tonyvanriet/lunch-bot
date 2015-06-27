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


(defn relative-date-str
  [date]
  (cond (= date (time/today)) nil
        (= date (time/minus (time/today) (time/days 1))) " yesterday"
        :else (str " on " date)))


(defn balances->str [balances]
  (str (str-coll (map #(str (person->str (key %)) " " (val %)) balances)) "\n"
       (let [bal-sum (apply + (vals balances))]
         (cond (= bal-sum 0M) (str "The balances are perfect!")
               (< bal-sum 0M) (str "We have an extra " (* -1 bal-sum))
               (> bal-sum 0M) (str "We're " bal-sum " short")))))

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
  (str (person->str person) " bought lunch for " amount (relative-date-str date)))

(defmethod event->str :cost [{:keys [person amount date]}]
  (str (person->str person) "'s lunch cost " amount (relative-date-str date)))

(defmethod event->str :uncost [{:keys [person amount date]}]
  (str "retracted " (person->str person) "'s lunch cost " amount (relative-date-str date)))

(defmethod event->str :paid [{:keys [person amount to date]}]
  (str (person->str person) " paid " (person->str to) " " amount (relative-date-str date)))

(defmethod event->str :borrowed [{:keys [person amount from date]}]
  (str (person->str person) " owes " (person->str from) " " amount (relative-date-str date)))

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


(defn recent-money-history [money-events]
  (let [min-date (time/minus (time/today) (time/days 7))
        recent-events (filter #(time/after? (:date %) min-date) money-events)
        recent-events-sorted (sort-by :date recent-events)]
    (if (seq recent-events-sorted)
      (str "History as of " min-date "\n"
           (events->str recent-events-sorted))
      (str "No history as of " min-date))))


(defn event->reply-str
  [event]
  (case (:type event)
    (:paid :borrowed :bought :cost :uncost :should-pay) (event->str event)
    :choose (let [{:keys [name menu-url]} (:restaurant event)]
              (str name " it is!" (when menu-url (str "\nHere's the menu: " menu-url))))
    :in (str (rand-nth [":metal:" ":rocket:" ":clap:" ":thumbsup:" ":dancers:" ":facepunch:"]))
    :out (str (rand-nth [":fu:" ":fire:" ":thumbsdown:" ":hankey:"]))
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
  (let [{:keys [chosen-restaurant-name ins outs orderless-ins]} (meal/summary meal)]
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
         "\n"
         (meal->orders-str meal))))

(defn post-order-summary
  [date meal]
  (let [{:keys [chosen-restaurant-name ins costless-ins buyers buyer-surplus]} (meal/summary meal)
        multiple-ins? (> (count ins) 1)
        buyers-str (people->str buyers)
        multiple-buyers? (> (count buyers) 1)]
    (str (if chosen-restaurant-name
           (str "Ordered from " chosen-restaurant-name (relative-date-str date))
           (str "No restaurant chosen")) "\n"
         (when (seq buyers)
           (str buyers-str " *bought*" "\n"))
         (when (seq ins)
           (str (people->str ins) " " (if multiple-ins? "were" "was") " *in*" "\n"))
         (if (seq costless-ins)
           (str "Waiting for the *cost* of " (people->str costless-ins) "'s lunch"
                (when (= (> (count costless-ins) 1) "es") "\n"))
           (str buyers-str " " (if multiple-buyers? "are" "is")
                (cond (= buyer-surplus 0M) (str " squared away")
                      (< buyer-surplus 0M) (str " " (- buyer-surplus) " ahead")
                      (> buyer-surplus 0M) (str " " buyer-surplus " short"))
                "\n")))))


(defn discrepant-meal-summary
  [date meal]
  (let [{:keys [chosen-restaurant-name buyer-surplus] :as summary} (meal/summary meal)]
    (str date (when chosen-restaurant-name (str ", " chosen-restaurant-name)) ", " buyer-surplus "\n"
         (apply str (for [in-person-meal (filter #(or (meal/person-in? meal (key %))
                                                      (meal/person-bought? meal (key %)))
                                                 (:people meal))]
                      (str in-person-meal "\n")))
         "\n")))

(defn discrepant-meals-summary
  [meals]
  (apply str (for [date-meal (sort-by key meals)]
               (discrepant-meal-summary (key date-meal) (val date-meal)))))


(defn person-meal->str
  [{:keys [order cost] :as meal-order}]
  (str "```" "order\n"
       order "\n"
       #_(when cost (str "cost " cost))
       "```\n"))

(defn person-meal-history
  [person-meals restaurant]
  (if (seq person-meals)
    (apply str "Your latest orders for " (:name restaurant) "\n"
           (map person-meal->str person-meals))
    (str "You haven't ordered from " (:name restaurant))))


(defn bought-nag
  [date]
  (str "If you bought lunch" (when (not= date (time/today)) (str " on " date)) ", let me know."))


(defn make-channel-message
  "build a message to be distributed to a particular channel"
  [channel-id text]
  {:distribution :channel, :channel-id channel-id, :text text})

(defn make-lunch-message
  "build a message to be distributed to the designated lunch channel"
  [text]
  {:distribution :broadcast, :text text})

(defn make-user-message
  "build a reply to be distributed to an individual user"
  [user-id text]
  {:distribution :user, :user-id user-id, :text text})


(defn say-message
  [channel-id text]
  (tx/say-message channel-id text))



