(ns lunch-bot.core-test
  (:require [clojure.test :refer :all]
            [lunch-bot.core :refer :all]
            [lunch-bot.money :as money]))


(def money-events

  [{:person "bob"
    :type :bought
    :amount 40}
   {:person "steve"
    :type :cost
    :amount 10}
   {:person "dude"
    :type :cost
    :amount 8}
   {:person "rozz"
    :type :cost
    :amount 12}
   {:person "bob"
    :type :cost
    :amount 10}

   {:person "steve"
    :type :paid
    :amount 10
    :recipient "bob"}
   {:person "dude"
    :type :paid
    :amount 8
    :recipient "bob"}

   {:person "bob"
    :type :bought
    :amount 20}
   {:person "steve"
    :type :cost
    :amount 7}
   {:person "rozz"
    :type :cost
    :amount 4}
   {:person "bob"
    :type :cost
    :amount 9}])


(deftest create-balances-from-events
  (let [balances (money/events->balances money-events)]
    (is (= (get balances "bob") 23))
    (is (= (get balances "steve") -7))
    (is (= (get balances "dude") 0))
    (is (= (get balances "rozz") -16))))


