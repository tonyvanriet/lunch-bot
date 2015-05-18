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


(deftest choose-retracts-previous-costs
  (let [today (time/today)
        restaurant {:name "BW3"}
        person-costed "U1234"
        person-choosing "U4321"
        cost-amount 2.34M
        aggs {:meals {today {:people            {person-costed {:status :in
                                                                :cost   cost-amount}}
                             :chosen-restaurant restaurant}}}]
    (testing "same restaurant chosen again, no choose event, no retractions"
      (let [choose-same-restaurant-cmd {:command-type :choose-restaurant
                                        :restaurant   restaurant
                                        :date         today
                                        :requestor    person-choosing}
            actual-events (command->events choose-same-restaurant-cmd aggs)]
        (is (= [] actual-events))))
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
               (command->events choose-different-restaurant-cmd aggs)))))))

