(ns lunch-bot.command.handler
  (:require
    [lunch-bot.talk :as talk]
    [lunch-bot.aggregate :as agg]
    [lunch-bot.money :as money]
    [lunch-bot.meal :as meal]
    [clj-time.core :as time]
    [clojure.core.incubator :refer [dissoc-in]])
  (:import (java.math RoundingMode)))


(def sales-tax-rate 0.0925M)


(defn assoc-if
  [map key val f]
  (if (f key val)
    (assoc map key val)
    map))

(defn assoc-if-some-val
  [map key val]
  (assoc-if map key val (fn [k v] (some? v))))

(defn base-command-event
  [{:keys [requestor ts] :as cmd}]
  (-> {}
      (assoc-if-some-val :person requestor)
      (assoc-if-some-val :ts ts)))


(defn apply-sales-tax
  "applies the sales-tax-rate to the amount if the events :+tax? is truthy,
   and then removes :+tax?"
  [event sales-tax-rate]
  (let [taxed-event (if (:+tax? event)
                      (update-in event [:amount] #(-> (* % (+ 1 sales-tax-rate))
                                                      (.setScale 2 RoundingMode/HALF_UP)))
                      event)]
    (dissoc-in taxed-event [:+tax?])))


(defn dispatch-command->events [cmd aggs] (:command-type cmd))

(defmulti command->events
          "converts the given command into events that should be committed to
          the event stream, if any."
          #'dispatch-command->events)


(defmethod command->events :default [_ _] nil)

(defmethod command->events :submit-payment
  [{:keys [amount to date] :as cmd} _]
  [(merge (base-command-event cmd)
          {:type   :paid
           :amount amount
           :to     to
           :date   date})])

(defmethod command->events :submit-bought
  [{:keys [amount date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        previous-bought-amount (get-in meal [:people requestor :bought])
        unbought-event (when previous-bought-amount
                         (merge (base-command-event cmd)
                                {:type   :unbought
                                 :amount previous-bought-amount
                                 :date   date}))
        bought-event (merge (base-command-event cmd)
                            {:type   :bought
                             :amount amount
                             :date   date})]
    (if unbought-event
      [unbought-event bought-event]
      [bought-event])))

(defmethod command->events :submit-cost
  [{:keys [amount +tax? date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        previous-cost-amount (get-in meal [:people requestor :cost])
        uncost-event (when previous-cost-amount
                       (merge (base-command-event cmd)
                              {:type   :uncost
                               :amount previous-cost-amount
                               :date   date}))
        pretax-cost-event (merge (base-command-event cmd)
                                 {:type   :cost
                                  :amount amount
                                  :+tax?  +tax?
                                  :date   date})
        cost-event (apply-sales-tax pretax-cost-event sales-tax-rate)]
    (if uncost-event
      [uncost-event cost-event]
      [cost-event])))

(defmethod command->events :declare-in
  [{:keys [date] :as cmd} _]
  [(merge (base-command-event cmd)
          {:type :in
           :date date})])

(defmethod command->events :declare-out
  [{:keys [date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        cost-amount (get-in meal [:people requestor :cost])
        uncost-event (when cost-amount
                       (merge (base-command-event cmd)
                              {:type   :uncost
                               :amount cost-amount
                               :date   date}))
        out-event (merge (base-command-event cmd)
                         {:type :out
                          :date date})]
    (if uncost-event
      [uncost-event out-event]
      [out-event])))

(defmethod command->events :choose-restaurant
  [{:keys [restaurant date] :as cmd} _]
  [(merge (base-command-event cmd)
          {:type       :choose
           :restaurant restaurant
           :date       date})])

(defmethod command->events :submit-order
  [{:keys [food date] :as cmd} _]
  [(merge (base-command-event cmd)
          {:type :order
           :food food
           :date date})])


(defn events->reply
  [events]
  (when (seq events)
    (->> events
         (map talk/event->reply-str)
         (interpose "\n")
         (apply str))))


(defn dispatch-command->reply [cmd events] ((juxt :command-type :info-type) cmd))

(defmulti command->reply
          "formulates the text reply to be returns for the command, if any."
          #'dispatch-command->reply)


(defmethod command->reply :default [_ _] nil)

(defmethod command->reply [:unrecognized nil]
  [_ _]
  "huh?")

(defmethod command->reply [:help nil]
  [_ _]
  (slurp "help.md"))

(defmethod command->reply [:show :balances]
  [_ _]
  (->> (agg/balances)
       (money/sort-balances)
       (reverse)
       (talk/balances->str)))

(defmethod command->reply [:show :pay?]
  [{:keys [requestor] :as cmd} _]
  (if-let [payment (money/best-payment requestor (agg/balances))]
    (talk/event->str payment)
    (str "Keep your money.")))

(defmethod command->reply [:show :payoffs]
  [_ _]
  (->> (agg/balances)
       (money/minimal-payoffs)
       (talk/payoffs->str)))

(defmethod command->reply [:show :history]
  [_ _]
  (->> (agg/money-events)
       (talk/recent-money-history)))

(defmethod command->reply [:show :meal-summary]
  [{:keys [date] :as cmd} _]
  (let [meals (agg/meals)
        meal (get meals date)]
    (if (or (time/before? date (time/today)) (meal/any-bought? meal))
      (talk/post-order-summary meal)
      (talk/pre-order-summary meal))))

(defmethod command->reply [:show :ordered?]
  [{:keys [requestor] :as cmd} _]
  (let [meals (agg/meals)
        todays-meal (get meals (time/today))]
    (if-let [todays-restaurant (-> todays-meal :chosen-restaurant)]
      (let [person-meals (meal/person-meal-history meals todays-restaurant requestor 3)]
        (talk/person-meal-history person-meals todays-restaurant))
      (str "Somebody needs to choose a restaurant first."))))

(defmethod command->reply [:show :discrepancies]
  [_ _]
  (let [meals (agg/meals)
        discrepant-meals (filter #(meal/is-discrepant (val %)) meals)]
    (talk/discrepant-meals-summary discrepant-meals)))


(defmethod command->reply [:submit-payment nil] [_ events] (events->reply events))
(defmethod command->reply [:submit-bought nil] [_ events] (events->reply events))
(defmethod command->reply [:submit-cost nil] [_ events] (events->reply events))
(defmethod command->reply [:declare-in nil] [_ events] (events->reply events))
(defmethod command->reply [:declare-out nil] [_ events] (events->reply events))
(defmethod command->reply [:choose-restaurant nil] [_ events] (events->reply events))
(defmethod command->reply [:submit-order nil] [_ events] (events->reply events))

