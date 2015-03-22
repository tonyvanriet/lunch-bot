(ns lunch-bot.meal-test
  (:require [clojure.test :refer :all]
            [lunch-bot.meal :refer :all]
            [clj-time.core :as time]))


(defn append-event [events event]
  (swap! events #(conj % event)))

(defn get-person-meal-today [events person]
  (get-in (events->meals events) [time/today :people person]))


(deftest meal-participation
  (let [meal-events (atom [])
        person "bob"
        order "dog and fries"
        cost-amount 2.34M]
    ; opportunity for a with-events macro here?
    ; to facilitate the accumulation of events throughout the test
    (testing "meal shows person in"
      (append-event meal-events {:type :in, :person person, :date time/today})
      (let [person-meal (get-person-meal-today @meal-events person)]
        (is (= :in (:status person-meal)))))
    (testing "meal shows order and cost"
      (append-event meal-events {:type :order, :person person, :date time/today, :food order})
      (append-event meal-events {:type :cost, :person person, :date time/today, :amount cost-amount})
      (let [person-meal (get-person-meal-today @meal-events person)]
        (is (= order (:order person-meal)))
        (is (= cost-amount (:cost person-meal)))))
    (testing "meal shows person out, no order and cost"
      (append-event meal-events {:type :out, :person person, :date time/today})
      (let [person-meal (get-person-meal-today @meal-events person)]
        (is (= :out (:status person-meal)))
        (is (not (contains? person-meal :order)))
        (is (not (contains? person-meal :cost)))
        (testing "no nils in meal aggregate"
          (is (not-any? nil? (vals person-meal))))))))
