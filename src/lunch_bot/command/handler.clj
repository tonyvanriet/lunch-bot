(ns lunch-bot.command.handler
  (:require [clojure.core.incubator :refer [dissoc-in]]
            [lunch-bot.meal :as meal])
  (:import (java.math RoundingMode)))


(def default-sales-tax-rate 0.0925M)


(defn assoc-if
  [map key val f]
  (if (f key val)
    (assoc map key val)
    map))

(defn assoc-if-some-val
  [map key val]
  (assoc-if map key val (fn [k v] (some? v))))


(defn make-event
  [{:keys [requestor ts] :as cmd} event-type with-map]
  (-> {:type event-type}
      (assoc-if-some-val :person requestor)
      (assoc-if-some-val :ts ts)
      (merge with-map)))


(defn calculate-sales-tax
  [amount rate]
  (-> amount
      (* rate)
      (.setScale 2 RoundingMode/HALF_UP)))


(defn apply-sales-tax
  "applies the sales-tax-rate to the amount if the events :+tax? is truthy,
   and then removes :+tax?"
  [event sales-tax-rate]
  (let [taxed-event (if (:+tax? event)
                      (-> event
                          (assoc :pretax-amount (:amount event))
                          (update-in [:amount] #(+ % (calculate-sales-tax % sales-tax-rate))))
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
  [(make-event cmd :paid {:amount amount
                          :to     to
                          :date   date})])

(defmethod command->events :submit-bought
  [{:keys [amount date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        previous-bought-amount (get-in meal [:people requestor :bought])
        unbought-event (when previous-bought-amount
                         (make-event cmd :unbought {:amount previous-bought-amount
                                                    :date   date}))
        bought-event (make-event cmd :bought {:amount amount
                                              :date   date})]
    (if unbought-event
      [unbought-event bought-event]
      [bought-event])))

(defmethod command->events :submit-cost
  [{:keys [amount +tax? date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        restaurant (:chosen-restaurant meal)
        sales-tax-rate (if (:sales-tax-rate restaurant)
                         (:sales-tax-rate restaurant)
                         default-sales-tax-rate)
        previous-cost-amount (get-in meal [:people requestor :cost])
        uncost-event (when previous-cost-amount
                       (make-event cmd :uncost {:amount previous-cost-amount
                                                :date   date}))
        pretax-cost-event (make-event cmd :cost {:amount amount
                                                 :+tax?  +tax?
                                                 :date   date})
        cost-event (apply-sales-tax pretax-cost-event sales-tax-rate)]
    (if uncost-event
      [uncost-event cost-event]
      [cost-event])))

(defmethod command->events :declare-in
  [{:keys [date] :as cmd} _]
  [(make-event cmd :in {:date date})])

(defmethod command->events :declare-out
  [{:keys [date requestor] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        cost-amount (get-in meal [:people requestor :cost])
        uncost-event (when cost-amount
                       (make-event cmd :uncost {:amount cost-amount
                                                :date   date}))
        out-event (make-event cmd :out {:date date})]
    (if uncost-event
      [uncost-event out-event]
      [out-event])))

(defmethod command->events :choose-restaurant
  [{:keys [restaurant date] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)]
    (if (= restaurant (:chosen-restaurant meal))
      []
      (let [choose-event (make-event cmd :choose {:restaurant restaurant
                                                  :date       date})
            costed-person-meals (filter #(contains? (val %) :cost) (:people meal))
            uncost-events (when (:chosen-restaurant meal)
                            (mapv #(make-event cmd :uncost {:amount (:cost (val %))
                                                            :date   date
                                                            :person (key %)})
                                  costed-person-meals))]
        (conj uncost-events choose-event)))))

(defmethod command->events :submit-order
  [{:keys [food date] :as cmd} _]
  [(make-event cmd :order {:food food
                           :date date})])


(defmethod command->events :find-nags
  [{:keys [date] :as cmd} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        meal-summary (meal/summary meal)
        costless-ins (:costless-ins meal-summary)
        boughtless? (not (seq (:buyers meal-summary)))]
    (when (or (seq costless-ins) boughtless?)
      [(make-event cmd :found-nags {:date         date
                                    :costless-ins costless-ins
                                    :boughtless?  boughtless?})])))

