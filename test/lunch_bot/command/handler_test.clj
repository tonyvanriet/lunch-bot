(ns lunch-bot.command.handler-test
  (:require [clojure.test :refer :all]
            [lunch-bot.command.handler :as handler]
            [clj-time.core :as time]))


(deftest cost-out-yields-cost-retracted
  (let [today (time/today)
        person "U1234"
        ts "1234.2345"
        cost-amount 2.34M
        out-cmd {:command-type :declare-out
                 :date         today
                 :requestor    person}
        expected-out-event {:type   :out
                            :date   (:date out-cmd)
                            :person (:requestor out-cmd)}]
    (testing "no previous cost, out yields no retraction"
      (let [aggs-no-cost {:meals {today {:people {person {:status :in}}}}}
            events (handler/command->events out-cmd aggs-no-cost)]
        (is (= events [expected-out-event]))))
    (testing "with previous cost, out yeilds cost retraction"
      (let [aggs-cost {:meals {today {:people {person {:status :in, :cost cost-amount}}}}}
            expected-uncost-event {:type   :uncost
                                   :amount cost-amount
                                   :date   today
                                   :person person}
            events (handler/command->events out-cmd aggs-cost)]
        (is (= events [expected-uncost-event expected-out-event]))))))

