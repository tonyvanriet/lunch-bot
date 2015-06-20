(ns lunch-bot.store-test
  (:require [clojure.test :refer :all]
            [lunch-bot.store :refer :all]
            [clj-time.core :as time]))


(defn orders-of-magnitude
  ([] (orders-of-magnitude 1))
  ([n] (cons n (lazy-seq (orders-of-magnitude (* 10 n))))))


#_(deftest test-event-write-scaling
  (let [event {:date   (time/local-date 2015 5 18),
               :amount 2M,
               :ts     "1434430870.000017",
               :person "U036B3Y6X"}]
    (doseq [n (take 6 (orders-of-magnitude))]
      (let [events (repeat n event)]
        (println "writing" n "events")
        (time (write-events events "test-event-write-scaling.edn"))))))

;writing 1 events
;"Elapsed time: 0.909016 msecs"
;writing 10 events
;"Elapsed time: 0.616698 msecs"
;writing 100 events
;"Elapsed time: 1.913247 msecs"
;writing 1000 events
;"Elapsed time: 17.063709 msecs"
;writing 10000 events
;"Elapsed time: 161.134748 msecs"
;writing 100000 events
;"Elapsed time: 1656.35519 msecs"