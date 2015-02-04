(ns lunch-bot.money-test
  (:require [clojure.test :refer :all]
            [lunch-bot.money :refer :all]))


(def events-single-buyer

  [{:person "bob" :type :bought :amount 40}
   {:person "steve" :type :cost :amount 10}
   {:person "dude" :type :cost :amount 8}
   {:person "rozz" :type :cost :amount 12}
   {:person "bob" :type :cost :amount 10}

   {:person "steve" :type :paid :amount 10 :to "bob"}
   {:person "dude" :type :paid :amount 8 :to "bob"}

   {:person "bob" :type :bought :amount 20}
   {:person "steve" :type :cost :amount 7}
   {:person "rozz" :type :cost :amount 4}
   {:person "bob" :type :cost :amount 9}])


(deftest create-balances-from-events
  (let [balances (events->balances events-single-buyer)]
    (doseq [[person amount] [["bob" 23]
                             ["steve" -7]
                             ["dude" 0]
                             ["rozz" -16]]]
      (is (= (get balances person) amount)))))


(def events-multi-buyers

  [{:person "bob" :type :bought :amount 40}
   {:person "steve" :type :cost :amount 10}
   {:person "dude" :type :cost :amount 8}
   {:person "rozz" :type :cost :amount 12}
   {:person "bob" :type :cost :amount 10}

   {:person "steve" :type :bought :amount 20}
   {:person "steve" :type :cost :amount 7}
   {:person "rozz" :type :cost :amount 4}
   {:person "bob" :type :cost :amount 9}

   {:person "rozz" :type :bought :amount 30}
   {:person "dude" :type :cost :amount 10}
   {:person "rozz" :type :cost :amount 10}
   {:person "bob" :type :cost :amount 10}])

(def balances-multi-buyers (events->balances events-multi-buyers))


(deftest minimal-payoffs-correct
  "a->b $7, a->c $5, b->c $5 becomes a->b $2, a->c $10"
  (let [initial-events [{:person "b" :type :paid :amount 7 :to "a"}
                        {:person "c" :type :paid :amount 5 :to "a"}
                        {:person "c" :type :paid :amount 5 :to "b"}]
        balances (events->balances initial-events)
        payoffs (minimal-payoffs balances)]
    ; is there some better way to test the values of a collection without regard for their order?
    ; I shouldn't have to explicitly check the count, and this pattern isn't reliable if any of
    ; the items are duplicated.
    (is (= (count payoffs) 2))
    (is (some #(= % {:person "a" :type :paid :amount 10 :to "c"}) payoffs))
    (is (some #(= % {:person "a" :type :paid :amount 2 :to "b"}) payoffs))))


(deftest minimal-payoffs-yield-zero-balances
  (let [balances balances-multi-buyers
        payoffs (minimal-payoffs balances)
        paid-off-balances (apply-events balances payoffs)]
    (is (every? #(= (val %) 0) paid-off-balances))))


(deftest get-balance-gets-correct-balance
  (let [balances balances-multi-buyers]
    (are [person balance]
      (= (get-balance balances person) balance)
      "bob" 11 "steve" 3 "dude" -18 "rozz" 4)))


(deftest consolidation-payments-correct
  (let [balances {"bob" 11, "steve" 3, "dude" -18, "rozz" 4}
        [payoff additional-payments] (consolidation-payments balances "dude" "bob")]
    (is (= payoff {:person "dude" :type :paid :amount 18 :to "bob"}))
    (is (= (count additional-payments) 2))
    (is (some #(= % {:person "bob" :type :paid :amount 3 :to "steve"}) additional-payments))
    (is (some #(= % {:person "bob" :type :paid :amount 4 :to "rozz"}) additional-payments))))