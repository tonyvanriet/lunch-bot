(ns lunch-bot.command-test
  (:require [clojure.test :refer :all]
            [lunch-bot.command :refer :all]
            [clj-time.core :as time]))


(deftest process-command-paid-reply-correct
  (let [payer "U1234"
        recipient "U2345"
        amount 34.5M
        text (str "paid <@" recipient "> " amount)]
    (is (= (message->command payer text) {:command-type :event
                                          :event        {:person payer
                                                         :type   :paid
                                                         :amount amount
                                                         :to     recipient
                                                         :date   (time/today)}}))))


(deftest process-command-unrecognized-reply-correct
  (is (= (message->command "U1234" "asdf") {:command-type :unrecognized})))


(deftest parse-amounts
  (are [word amount]
    (= (word->amount word) amount)
    "2.34" 2.34M
    "5" 5.0M
    "$2345.67" 2345.67M
    ".50" 0.5M
    "55.123" nil
    "1,234" 1234.0M
    "$1,234.56" 1234.56M
    "12." nil
    "." nil))

