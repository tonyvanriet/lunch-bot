(ns lunch-bot.aggregate-test
  (:require [clojure.test :refer :all]
            [lunch-bot.aggregate :refer :all]
            [lunch-bot.event :as event]
            [clj-time.core :as time]))


(deftest cost-retraction-correct-in-aggregates
  (let [person "U1234"
        date (time/today)
        cost-amount 3.45M
        cost-event {:type   :cost
                    :amount cost-amount
                    :date   date
                    :person person}
        uncost-event {:type   :uncost
                      :amount cost-amount
                      :date   date
                      :person person}
        out-event {:type   :out
                   :date   date
                   :person person}]
    (testing "after cost, balances and meal reflect cost"
      (with-redefs [event/get-committed-events (constantly [cost-event])]
        (is (= (* -1 cost-amount) (get (balances) person)))
        (is (= cost-amount (get-in (meals) [date :people person :cost])))))
    (testing "after out, balances and meal reflect retraction"
      (with-redefs [event/get-committed-events (constantly [cost-event uncost-event out-event])]
        (is (= 0M (get (balances) person)))
        (is (not (contains? (get-in (meals) [date person]) :cost)))))))


(deftest repeat-bought-correct-in-aggregates
  (let [person "U1234"
        date (time/today)
        first-bought-amount 34.56M
        second-bought-amount 45.67M
        first-bought-event {:type   :bought
                            :amount first-bought-amount
                            :date   date
                            :person person}
        unbought-event {:type   :unbought
                        :amount first-bought-amount
                        :date   date
                        :person person}
        second-bought-event {:type   :bought
                             :amount second-bought-amount
                             :date   date
                             :person person}]
    (testing "after first bought, balances and meal reflect bought"
      (with-redefs [event/get-committed-events (constantly [first-bought-event])]
        (is (= first-bought-amount (get (balances) person)))
        (is (contains? (get-in (meals) [date :people person]) :bought))
        (is (= first-bought-amount (get-in (meals) [date :people person :bought])))))
    (testing "after unbought, balances and meal reflect retraction"
      (with-redefs [event/get-committed-events (constantly [first-bought-event unbought-event])]
        (is (= 0M (get (balances) person)))
        (is (not (contains? (get-in (meals) [date :people person]) :bought)))))
    (testing "after second bought, balances and meal reflect second bought"
      (with-redefs [event/get-committed-events (constantly [first-bought-event unbought-event second-bought-event])]
        (is (= second-bought-amount (get (balances) person)))
        (is (= second-bought-amount (get-in (meals) [date :people person :bought])))))))


(deftest clear-orders-when-restaurant-choice-changes
  (let [person "U1234"
        date (time/today)
        order-event {:type   :order
                     :food   "things and stuff"
                     :date   date
                     :person person}
        choose-event {:type       :choose
                      :restaurant {:name "BW3"}
                      :date       date}
        init-events [choose-event order-event]]
    (testing "restaurant chosen and orders submitted"
      (with-redefs [event/get-committed-events (constantly init-events)]
        (is (= (:restaurant choose-event) (get-in (meals) [date :chosen-restaurant])))
        (is (= (:food order-event) (get-in (meals) [date :people person :order])))))
    (testing "different restaurant chosen, orders cleared"
      (let [second-choose-event {:type       :choose
                                 :restaurant {:name "Chipotle"}
                                 :date       date}]
        (with-redefs [event/get-committed-events (constantly (conj init-events second-choose-event))]
          (is (= (:restaurant second-choose-event) (get-in (meals) [date :chosen-restaurant])))
          (is (contains? (get-in (meals) [date :people]) person))
          (is (not (contains? (get-in (meals) [date :people person]) :order))))))))
