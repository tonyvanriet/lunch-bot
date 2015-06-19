(ns lunch-bot.command.reply-test
  (:require [clojure.test :refer :all]
            [lunch-bot.command.reply :refer :all]
            [clj-time.core :as time]))


(deftest nag-replies-to-costless-ins
  (let [today (time/today)
        person "U1234"
        nag-cmd {:command-type :send-nags
                 :date         (time/today)}
        aggs {:meals {today {:people {person {:status :in, :bought 23.45M}}}}}
        replies (command->replies nag-cmd aggs nil)
        reply (first replies)]
    (is (= 1 (count replies)))
    (is (= :user (:distribution reply)))
    (is (= person (:user-id  reply)))))




