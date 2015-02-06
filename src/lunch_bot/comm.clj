(ns lunch-bot.comm
  (:require [clojure.string :as str]))


;;
;; Parsing user commands
;;
;; the input string is broken into words.
;; each word is classified as a particular kind of command element.
;; command elements can be:
;;   specific keywords (:paid, :bought, :cost, :balances, :payoffs, :events)
;;   the target of a keyword (:user, :restaurant)
;;   amount (:amount)
;;   date (:date)
;;   unnecessary text (:filler)
;;
;; the list of command elements is a command template.
;; the command template is compared against a prioritized list of
;; recognized command templates.
;; if a match is found, the matching command template provides a function
;; that parses specific information out of the command (user-ids, actual
;; amounts, dates) and then calls out to some other part of the system
;; to perform the corresponding action.
;; if no match is found, it's considered an invalid command.
;;   could recognize partial command templates give the user an error
;;   message indicating the unrecognized parts.
;;



(defn word->user-id
  [word]
  (when-let [[_ user-id] (re-find #"<@(U\w+)(?:\|\w+)?>" word)]
    user-id))


(def command-keyword-strs ["paid" "bought" "cost" "show"])

(def noun-strs ["balances" "payoffs" "events"])


(defn word->keyword
  [word keyword-strs]
  (let [lword (.toLowerCase word)]
    (when-let [keyword-str (some #(when (.startsWith % lword) %) keyword-strs)]
      (keyword keyword-str))))


(defn word->command-keyword
  [word]
  (word->keyword word command-keyword-strs))

(defn word->noun
  [word]
  (word->keyword word noun-strs))


(defn word->amount
  [word]
  ;; regex is a modified version of this answer on SO
  ;; http://stackoverflow.com/questions/354044/what-is-the-best-u-s-currency-regex
  (when-let [[_ amount-str] (re-find #"^([+-]?\$?([0-9]{1,3}(?:,?[0-9]{3})*|[0-9]*)(?:\.[0-9]{1,2})?)$" word)]
    (-> amount-str
        (str/replace #",|\$" "")
        (BigDecimal.))))


(defn word->command-element
  [word]
  (let [cmd-keyword (word->command-keyword word)]
    (cond cmd-keyword cmd-keyword
          (word->noun word) :noun
          (word->user-id word) :user
          (word->amount word) :amount)))


(defn command-template->func
  [command-template]
  (cond

    (= command-template [:paid :user :amount])
    (fn [words commander]
      {:command-type :event
       :event        {:person commander
                      :type   :paid
                      :amount (word->amount (nth words 2))
                      :to     (word->user-id (nth words 1))}})

    ;; TODO: (= command-template [:paid :amount :user])

    (= command-template [:noun])
    (fn [words _]
      {:command-type :show
       :info-type    (word->noun (first words))})))


(defn text->command-func
  [text]
  (let [words (str/split text #" +")
        command-template (map #(word->command-element %) words)]
    (when-let [cmd-func (command-template->func command-template)]
      (fn [commander] (cmd-func words commander)))))


(defn message->command
  "parses the message text and returns a command map"
  [user-id text]
  (let [cmd-func (text->command-func text)]
    (if cmd-func
      (cmd-func user-id)
      {:command-type :unrecognized})))