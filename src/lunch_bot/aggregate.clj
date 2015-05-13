(ns lunch-bot.aggregate
  (:require
    [lunch-bot.event :as event]
    [lunch-bot.meal :as meal]
    [lunch-bot.money :as money]))


(defn balances []
  (money/events->balances @event/committed-events))


(defn meals []
  (->> @event/committed-events
       (sort-by :ts)
       (meal/events->meals)))


(defn money-events []
  (->> @event/committed-events
       (filter money/money-event?)))