(ns lunch-bot.command
  (:require [clojure.string :as str]
            [clj-time.core :as tc]
            [clj-time.format :as tf]))



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



(def command-keyword-strs ["paid" "bought" "cost" "show" "undo"])

(def noun-strs ["balances" "payoffs" "events"])

(def filler-strs ["lunch" "for" "i" "my"])

(def relative-date-strs ["today" "yesterday"])

(defn word->keyword
  [word keyword-strs]
  (let [lword (.toLowerCase word)]
    (when-let [keyword-str (some #(when (.startsWith % lword) %) keyword-strs)]
      (keyword keyword-str))))

(defn word->command-keyword
  [word]
  (word->keyword word command-keyword-strs))

(defn word->user-id
  [word]
  (when-let [[_ user-id] (re-find #"<@(U\w+)(?:\|\w+)?>" word)]
    user-id))

(defn word->noun
  [word]
  (word->keyword word noun-strs))

(defn filler?
  [word]
  (let [lword (.toLowerCase word)]
    (some #(.startsWith % lword) filler-strs)))

(defn word->amount
  [word]
  ;; regex is a modified version of this answer on SO
  ;; http://stackoverflow.com/questions/354044/what-is-the-best-u-s-currency-regex
  (when-let [[_ amount-str] (re-find #"^([+-]?\$?([0-9]{1,3}(?:,?[0-9]{3})*|[0-9]*)(?:\.[0-9]{1,2})?)$" word)]
    (-> amount-str
        (str/replace #",|\$" "")
        (BigDecimal.))))

(def date-formatter
  (tf/formatter (tc/default-time-zone) "YYYYMMdd" "YYYY-MM-dd" "YYYY/MM/dd"))

(defn word->relative-date
  [word]
  (word->keyword word relative-date-strs))

(defn relative-date->date
  [relative-date]
  (case relative-date
    :today (tc/today)
    :yesterday (tc/minus (tc/today) (tc/days 1))            ; for some reason, tc/yesterday has a time
    :default nil))

(defn word->date
  [word]
  (if-let [relative-date (word->relative-date word)]
    (relative-date->date relative-date)
    #_(try
      (tf/parse date-formatter word)
      (catch IllegalArgumentException ex
        nil))))

(defn word->command-element
  [word]
  (if-let [cmd-keyword (word->command-keyword word)]
    [cmd-keyword cmd-keyword]
    (if-let [noun (word->noun word)]
      [:noun noun]
      (if-let [user-id (word->user-id word)]
        [:user user-id]
        (if-let [amount (word->amount word)]
          [:amount amount]
          (if-let [date (word->date word)]
            [:date date]
            (if (filler? word)
              [:filler word]
              nil)))))))


(defn remove-filler
  [command-template]
  (filter #(not (= (first %) :filler)) command-template))


(defn command-template->key
  [template]
  (map #(first %) template))


(defmulti command-template->func command-template->key)

(defmethod command-template->func :default [_]
  nil)

(defmethod command-template->func [:noun]
  [[[_ noun]]]
  (fn [_]
    {:command-type :show
     :info-type    noun}))

(defmethod command-template->func [:paid :user :amount]
  [[[_ event-type] [_ user-id] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   event-type
                    :amount amount
                    :to     user-id
                    :on     (relative-date->date :today)}}))

(defmethod command-template->func [:paid :amount :user]
  [[event-elem amount-elem user-elem]]
  (command-template->func [event-elem user-elem amount-elem]))

(defmethod command-template->func [:bought :date :amount]
  [[[_ event-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   event-type
                    :amount amount
                    :on     date}}))

(defmethod command-template->func [:bought :amount :date]
  [[event-elem amount-elem date-elem]]
  (command-template->func [event-elem date-elem amount-elem]))

(defmethod command-template->func [:bought :amount]
  [[event-elem amount-elem]]
  (command-template->func [event-elem [:date (relative-date->date :today)] amount-elem]))

(defmethod command-template->func [:cost :date :amount]
  [[[_ event-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   event-type
                    :amount amount
                    :on     date}}))

(defmethod command-template->func [:cost :amount :date]
  [[event-elem amount-elem date-elem]]
  (command-template->func [event-elem date-elem amount-elem]))

(defmethod command-template->func [:cost :amount]
  [[event-elem amount-elem]]
  (command-template->func [event-elem [:date (relative-date->date :today)] amount-elem]))


(defn text->command-func
  [text]
  (let [words (str/split text #" +")
        cmd-template (map #(word->command-element %) words)
        clean-cmd-template (remove-filler cmd-template)]
    (command-template->func clean-cmd-template)))


(defn message->command
  "parses the message text and returns a command map"
  [user-id text]
  (let [cmd-func (text->command-func text)]
    (if cmd-func
      (cmd-func user-id)
      {:command-type :unrecognized})))