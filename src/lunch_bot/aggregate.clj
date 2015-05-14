(ns lunch-bot.aggregate
  (:require
    [lunch-bot.event :as event]
    [lunch-bot.meal :as meal]
    [lunch-bot.money :as money]))


(defn balances []
  (money/events->balances (event/get-committed-events)))


(defn meals []
  (->> (event/get-committed-events)
       (sort-by :ts)
       (meal/events->meals)))


(defn money-events []
  (filter money/money-event? (event/get-committed-events)))


(defn get-aggregates []
  {:committed-events (event/get-committed-events)
   :balances         (balances)
   :meals            (meals)
   :money-events     (money-events)})
