(ns lunch-bot.comm-test
  (:require [clojure.test :refer :all]
            [lunch-bot.comm :refer :all]))


(deftest process-command-paid-reply-correct
  (let [payer "U1234"
        recipient "U2345"
        amount 34.5
        text (str "paid <@" recipient "> " amount)
        reply (process-command "D234" payer text)]
    (is (= (read-string reply) {:person    payer
                                :type      :paid
                                :amount    amount
                                :recipient recipient}))))


(deftest process-command-unrecognized-reply-correct
  (is (= (process-command "D234" "U1234" "asdf") "huh?")))

