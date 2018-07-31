(ns lunch-bot.command-test
  (:require [clojure.test :refer :all]
            [lunch-bot.command :refer :all]
            [lunch-bot.command.parse :refer :all]
            [lunch-bot.command.template :refer :all]
            [clj-time.core :as time]
            [clj-slack-client.team-state :as team]))


(deftest process-command-paid-reply-correct
  (let [payer "U1234"
        recipient "U2345"
        amount 34.5M
        text (str "paid <@" recipient "> " amount)]
    (with-redefs [team/name->id (fn [name] name)]
      (is (= (command-text->command text) {:command-type :submit-payment
                                           :amount       amount
                                           :to           recipient
                                           :date         (time/today)})))))


(deftest process-command-unrecognized-reply-correct
  (is (= (command-text->command "asdf") {:command-type :unrecognized})))


(deftest process-command-number-of-diners
  (is (= (command-text->command  "12 people")
         {:command-type :declare-diners
          :number 12
          :date (time/today)}))
  (is (= (command-text->command "2.5 people")
         {:command-type :declare-diners
          :number 2    ; it casts to int
          :date (time/today)})))


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
    "." nil
    "" nil))


(deftest cost-command-realization
  (let [amount 12.34M
        date (time/local-date 2015 5 1)
        expected-command {:command-type :submit-cost
                          :amount       amount
                          :+tax?        :+tax
                          :date         date}]
    (testing "cost amount +tax date yields submit-cost command"
      (is (= expected-command
             (command-template->command [[:cost :cost]
                                         [:amount amount]
                                         [:+tax :+tax]
                                         [:date date]]))))
    (testing "cost date amount +tax yields submit-cost command"
      (is (= expected-command
             (command-template->command [[:cost :cost]
                                         [:date date]
                                         [:amount amount]
                                         [:+tax :+tax]]))))))


(deftest owe-command-realization
  (let [from-person "U1234"
        amount 12.34M
        date (time/today)
        expected-command {:command-type :submit-debt
                          :from         from-person
                          :amount       amount
                          :date         date}]
    (testing "owe person amount yields submit-debt command"
      (is (= expected-command
             (command-template->command [[:borrowed :borrowed]
                                         [:user from-person]
                                         [:amount amount]]))))
    (testing "owe amount person yields submit-debt command"
      (is (= expected-command
             (command-template->command [[:borrowed :borrowed]
                                         [:amount amount]
                                         [:user from-person]]))))))


