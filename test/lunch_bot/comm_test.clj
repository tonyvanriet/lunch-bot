(ns lunch-bot.comm-test
  (:require [clojure.test :refer :all]
            [lunch-bot.comm :refer :all]))


(deftest process-command-paid-reply-correct
  (let [payer "U1234"
        recipient "U2345"
        amount 34.5
        text (str "paid <@" recipient "> " amount)
        reply (process-command "D234" payer text)]
    (is (= (read-string reply) {:command-type :event
                                :event        {:person    payer
                                               :type      :paid
                                               :amount    amount
                                               :recipient recipient}}))))


(deftest process-command-unrecognized-reply-correct
  (is (= (process-command "D234" "U1234" "asdf") "huh?")))


(deftest parse-amounts
  (are [word amount]
    (= (word->amount word) amount)
    "2.34" 2.34
    "5" 5.0
    "$2345.67" 2345.67
    ".50" 0.5
    "55.123" nil
    "1,234" 1234.0
    "$1,234.56" 1234.56
    "12." nil
    "." nil))

