(ns lunch-bot.command.handler-test
  (:require [clojure.test :refer :all]
            [lunch-bot.command.handler :refer :all]
            [clj-time.core :as time]))


(deftest cost-out-yields-cost-retracted
  (let [today (time/today)
        person "U1234"
        cost-amount 2.34M
        out-cmd {:command-type :declare-out
                 :date         today
                 :requestor    person}
        expected-out-event {:type   :out
                            :date   today
                            :person person}]
    (testing "no previous cost, out yields no retraction"
      (let [aggs-no-cost {:meals {today {:people {person {:status :in}}}}}
            events (command->events out-cmd aggs-no-cost)]
        (is (= events [expected-out-event]))))
    (testing "with previous cost, out command yields cost retraction event"
      (let [aggs-cost {:meals {today {:people {person {:status :in, :cost cost-amount}}}}}
            expected-uncost-event {:type   :uncost
                                   :amount cost-amount
                                   :date   today
                                   :person person}
            events (command->events out-cmd aggs-cost)]
        (is (= events [expected-uncost-event expected-out-event]))))))


(deftest repeat-cost-retracts-previous-cost
  (let [today (time/today)
        person "U1234"
        first-cost-amount 34.56M]
    (testing "no previous cost, no retraction"
      (let [aggs-no-cost {:meals {today {:people {person {:status :in}}}}}
            first-cost-cmd {:command-type :submit-cost
                            :amount       first-cost-amount
                            :date         today
                            :requestor    person}
            expected-first-cost-event {:type   :cost
                                       :amount first-cost-amount
                                       :date   today
                                       :person person}
            events (command->events first-cost-cmd aggs-no-cost)]
        (is (= events [expected-first-cost-event]))))
    (testing "second cost command yields retraction event"
      (let [aggs-cost {:meals {today {:people {person {:status :in, :cost first-cost-amount}}}}}
            second-cost-amount 45.67M
            second-cost-cmd {:command-type :submit-cost
                             :amount       second-cost-amount
                             :date         today
                             :requestor    person}
            expected-uncost-event {:type   :uncost
                                   :amount first-cost-amount
                                   :date   today
                                   :person person}
            expected-second-cost-event {:type   :cost
                                        :amount second-cost-amount
                                        :date   today
                                        :person person}
            events (command->events second-cost-cmd aggs-cost)]
        (is (= events [expected-uncost-event expected-second-cost-event]))))))


(deftest repeat-bought-retracts-previous-bought
  (let [today (time/today)
        person "U1234"
        first-bought-amount 34.56M]
    (testing "no previous bought, no retraction"
      (let [aggs-no-bought {:meals {today {:people {person {:status :in}}}}}
            first-bought-cmd {:command-type :submit-bought
                              :amount       first-bought-amount
                              :date         today
                              :requestor    person}
            expected-first-bought-event {:type   :bought
                                         :amount first-bought-amount
                                         :date   today
                                         :person person}
            events (command->events first-bought-cmd aggs-no-bought)]
        (is (= events [expected-first-bought-event]))))
    (testing "second bought command yields retraction event"
      (let [aggs-bought {:meals {today {:people {person {:status :in, :bought first-bought-amount}}}}}
            second-bought-amount 45.67M
            second-bought-cmd {:command-type :submit-bought
                               :amount       second-bought-amount
                               :date         today
                               :requestor    person}
            expected-unbought-event {:type   :unbought
                                     :amount first-bought-amount
                                     :date   today
                                     :person person}
            expected-second-bought-event {:type   :bought
                                          :amount second-bought-amount
                                          :date   today
                                          :person person}
            events (command->events second-bought-cmd aggs-bought)]
        (is (= events [expected-unbought-event expected-second-bought-event]))))))


(deftest restaurant-change-retracts-previous-costs
  (let [today (time/today)
        restaurant {:name "BW3"}
        person-costed "U1234"
        person-choosing "U4321"
        cost-amount 2.34M
        aggs-no-choice {:meals {today {:people {person-costed {:status :in
                                                               :cost   cost-amount}}}}}
        aggs-chosen (assoc-in aggs-no-choice [:meals today :chosen-restaurant] restaurant)
        choose-restaurant-cmd {:command-type :choose-restaurant
                               :restaurant   restaurant
                               :date         today
                               :requestor    person-choosing}]
    (testing "first restaurant choice does not retract costs"
      (let [choose-event {:type       :choose
                          :restaurant restaurant
                          :date       today
                          :person     person-choosing}]
        (is (= [choose-event] (command->events choose-restaurant-cmd aggs-no-choice)))))
    (testing "same restaurant chosen again yields no choose event and no retractions"
      (is (= [] (command->events choose-restaurant-cmd aggs-chosen))))
    (testing "different restaurant chosen, costs retracted"
      (let [different-restaurant {:name "Portillo's"}
            choose-different-restaurant-cmd {:command-type :choose-restaurant
                                             :restaurant   different-restaurant
                                             :date         today
                                             :requestor    person-choosing}
            expected-uncost-event {:type   :uncost
                                   :amount cost-amount
                                   :person person-costed
                                   :date   today}
            expected-choose-event {:type       :choose
                                   :restaurant different-restaurant
                                   :date       today
                                   :person     person-choosing}]
        (is (= [expected-uncost-event expected-choose-event]
               (command->events choose-different-restaurant-cmd aggs-chosen)))))))


(deftest restaurant-with-custom-sales-tax-overrides-default
  (let [today (time/today)
        cost-amount 5.67M
        default-rate default-sales-tax-rate
        custom-rate (+ default-sales-tax-rate 0.01M)
        cost-plus-tax-cmd {:command-type :submit-cost
                           :amount       cost-amount
                           :+tax?        :+tax
                           :date         today}]
    (testing "cost for restaurant without custom sales tax yields cost event with default sales tax"
      (let [aggs-restaurant-without-tax {:meals {today {:chosen-restaurant {:name "BW3"}}}}
            expected-cost-event-default {:type          :cost
                                         :pretax-amount cost-amount
                                         :amount        (+ cost-amount
                                                           (calculate-sales-tax cost-amount default-rate))
                                         :date          today}]
        (is (= [expected-cost-event-default]
               (command->events cost-plus-tax-cmd aggs-restaurant-without-tax)))))
    (testing "cost for restaurant with custom sales tax yields cost event with custom sales tax"
      (let [aggs-restaurant-with-tax {:meals {today {:chosen-restaurant {:name           "BW3"
                                                                         :sales-tax-rate custom-rate}}}}
            expected-cost-event-custom {:type          :cost
                                        :pretax-amount cost-amount
                                        :amount        (+ cost-amount
                                                          (calculate-sales-tax cost-amount custom-rate))
                                        :date          today}]
        (is (= [expected-cost-event-custom]
               (command->events cost-plus-tax-cmd aggs-restaurant-with-tax)))))))
