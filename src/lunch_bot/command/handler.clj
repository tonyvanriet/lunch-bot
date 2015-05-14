(ns lunch-bot.command.handler
  (:require
    [lunch-bot.talk :as talk]
    [lunch-bot.aggregate :as agg]
    [lunch-bot.money :as money]
    [lunch-bot.meal :as meal]
    [clj-time.core :as time]
    [clojure.core.incubator :refer [dissoc-in]])
  (:import (java.math RoundingMode)))


(defn dispatch-command->events [cmd] (:command-type cmd))

(defmulti command->events
          "converts the given command into events that should be committed to
          the event stream, if any."
          #'dispatch-command->events)


(defmethod command->events :default [_] nil)

(defmethod command->events :submit-payment
  [{:keys [amount to date] :as cmd}]
  [{:type   :paid
    :amount amount
    :to     to
    :date   date}])

(defmethod command->events :submit-bought
  [{:keys [amount date] :as cmd}]
  [{:type   :bought
    :amount amount
    :date   date}])

(defmethod command->events :submit-cost
  [{:keys [amount date +tax?] :as cmd}]
  [{:type   :cost
    :amount amount
    :+tax?  +tax?
    :date   date}])

(defmethod command->events :declare-in
  [{:keys [date] :as cmd}]
  [{:type :in
    :date date}])

(defmethod command->events :declare-out
  [{:keys [date] :as cmd}]
  [{:type :out
    :date date}])

(defmethod command->events :choose-restaurant
  [{:keys [restaurant date] :as cmd}]
  [{:type       :choose
    :restaurant restaurant
    :date       date}])

(defmethod command->events :submit-order
  [{:keys [food date] :as cmd}]
  [{:type :order
    :food food
    :date date}])


(defn contextualize-event
  "adds slack message context to event"
  [event {user-id :user, ts :ts, :as msg}]
  (-> event (assoc :person user-id) (assoc :ts ts)))

(defn apply-sales-tax
  "applies the sales-tax-rate to the amount if the events :+tax? is truthy,
   and then removes :+tax?"
  [event sales-tax-rate]
  (let [taxed-event (if (:+tax? event)
                      (update-in event [:amount] #(-> (* % (+ 1 sales-tax-rate))
                                                      (.setScale 2 RoundingMode/HALF_UP)))
                      event)]
    (dissoc-in taxed-event [:+tax?])))

(defn condition-event
  [event msg sales-tax-rate]
  (-> event
      (contextualize-event msg)
      (apply-sales-tax sales-tax-rate)))


(defn events->reply
  [events]
  (when (seq events)
    (->> events
         (map talk/event->reply-str)
         (interpose "\n")
         (apply str))))


(defn dispatch-command->reply [cmd msg events] ((juxt :command-type :info-type) cmd))

(defmulti command->reply
          "formulates the text reply to be returns for the command, if any."
          #'dispatch-command->reply)


(defmethod command->reply :default [_ _ _] nil)

(defmethod command->reply [:unrecognized nil]
  [_ _ _]
  "huh?")

(defmethod command->reply [:help nil]
  [_ _ _]
  (slurp "help.md"))

(defmethod command->reply [:show :balances]
  [_ _ _]
  (->> (agg/balances)
       (money/sort-balances)
       (reverse)
       (talk/balances->str)))

(defmethod command->reply [:show :pay?]
  [_ {requestor :user} _]
  (if-let [payment (money/best-payment requestor (agg/balances))]
    (talk/event->str payment)
    (str "Keep your money.")))

(defmethod command->reply [:show :payoffs]
  [_ _ _]
  (->> (agg/balances)
       (money/minimal-payoffs)
       (talk/payoffs->str)))

(defmethod command->reply [:show :history]
  [_ _ _]
  (->> (agg/money-events)
       (talk/recent-money-history)))

(defmethod command->reply [:show :meal-summary]
  [{:keys [date] :as cmd} _ _]
  (let [meals (agg/meals)
        meal (get meals date)]
    (if (or (time/before? date (time/today)) (meal/any-bought? meal))
      (talk/post-order-summary meal)
      (talk/pre-order-summary meal))))

(defmethod command->reply [:show :ordered?]
  [_ {requestor :user} _]
  (let [meals (agg/meals)
        todays-meal (get meals (time/today))]
    (if-let [todays-restaurant (-> todays-meal :chosen-restaurant)]
      (let [person-meals (meal/person-meal-history meals todays-restaurant requestor 3)]
        (talk/person-meal-history person-meals todays-restaurant))
      (str "Somebody needs to choose a restaurant first."))))

(defmethod command->reply [:show :discrepancies]
  [_ _ _]
  (let [meals (agg/meals)
        discrepant-meals (filter #(meal/is-discrepant (val %)) meals)]
    (talk/discrepant-meals-summary discrepant-meals)))


(defmethod command->reply [:submit-payment nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:submit-bought nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:submit-cost nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:declare-in nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:declare-out nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:choose-restaurant nil] [cmd msg events] (events->reply events))
(defmethod command->reply [:submit-order nil] [cmd msg events] (events->reply events))

