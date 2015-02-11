(ns lunch-bot.command
  (:require [clojure.string :as str]
            [clj-time.core :as tc]
            [clj-time.format :as tf]
            [clojure.math.combinatorics :as combo]))

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

(def action-strs ["paid" "bought" "cost" "show" "undo" "restaurant" "choose" "order" "in" "out"])

(def noun-strs ["balances" "payoffs" "events"])

(def filler-strs ["lunch" "for" "i" "my" "i'm"])

(def relative-date-strs ["today" "yesterday"])


(defn first-starts-with
  [word strs]
  (some #(when (.startsWith % word) %) strs))

(defn word->keyword
  "finds the first str in strs that starts with the word and returns
  it as a keyword."
  [word strs]
  (-> word
      (.toLowerCase)
      (first-starts-with strs)
      (keyword)))

(defn word->action
  [word]
  (word->keyword word action-strs))

(defn word->user-id
  [word]
  (when-let [[_ user-id] (re-find #"<@(U\w+)(?:\|\w+)?>" word)]
    user-id))

(defn word->noun
  [word]
  (word->keyword word noun-strs))

(defn word->filler
  [word]
  (-> word
      (.toLowerCase)
      (first-starts-with filler-strs)))


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
    :yesterday (tc/minus (tc/today) (tc/days 1))            ; tc/yesterday returns (now - 1 day) as a DateTime
    :default nil))

(defn word->date
  [word]
  (if-let [relative-date (word->relative-date word)]
    (relative-date->date relative-date)
    #_(try
      (tf/parse date-formatter word)
      (catch IllegalArgumentException ex
        nil))))

(defn word->food
  [word]
  word)


(defn get-today
  []
  (relative-date->date :today))


(def ^:private restaurants (atom []))
(swap! restaurants (fn [_] [{:name "BW3"}
                            {:name "Chipotle"}
                            {:name "Binny's"}]))

(defn word->restaurant
  "returns the first restaurant for which the name starts with the word"
  [word]
  (let [lword (.toLowerCase word)]
    (first (filter #(-> (:name %)
                        (.toLowerCase)
                        (.startsWith lword))
                   @restaurants))))


(defn word->command-elements
  "runs the word through all element parsers and returns all
  elements with non-nil values"
  [word]
  (let [action-keyword (word->action word)
        elements [[action-keyword action-keyword]
                  [:noun (word->noun word)]
                  [:user (word->user-id word)]
                  [:amount (word->amount word)]
                  [:date (word->date word)]
                  [:restaurant (word->restaurant word)]
                  [:food (word->food word)]
                  [:filler (word->filler word)]]]
    (filter second elements)))


(defn remove-filler
  [command-template]
  (filter #(not (= (first %) :filler)) command-template))


(defn merge-food-elements
  "combine consecutive food elements into a single element"
  [command-template]
  (reduce (fn [template element]
            (if (= :food (first (last template)) (first element))
              (let [last-food (second (last template))
                    next-food (second element)]
                (conj (vec (butlast template)) [:food (str last-food " " next-food)]))
              (conj template element)))
          []
          command-template))


(defn command-template->element-keys
  [template]
  (map #(first %) template))


(defmulti command-template->func command-template->element-keys)

(defmethod command-template->func :default [_]
  nil)

(defmethod command-template->func [:noun]
  [[[_ noun]]]
  (fn [_]
    {:command-type :show
     :info-type    noun}))

(defmethod command-template->func [:paid :user :amount]
  [[[_ action-type] [_ user-id] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :to     user-id
                    :date   (get-today)}}))

(defmethod command-template->func [:paid :amount :user]
  [[action-elem amount-elem user-elem]]
  (command-template->func [action-elem user-elem amount-elem]))

(defmethod command-template->func [:bought :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :date   date}}))

(defmethod command-template->func [:bought :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->func [action-elem date-elem amount-elem]))

(defmethod command-template->func [:bought :amount]
  [[action-elem amount-elem]]
  (command-template->func [action-elem [:date (get-today)] amount-elem]))

(defmethod command-template->func [:cost :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :date   date}}))

(defmethod command-template->func [:cost :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->func [action-elem date-elem amount-elem]))

(defmethod command-template->func [:cost :amount]
  [[action-elem amount-elem]]
  (command-template->func [action-elem [:date (get-today)] amount-elem]))

(defmethod command-template->func [:choose :restaurant]
  [[[_ action-type] [_ restaurant]]]
  (fn [_]
    {:command-type :meal-event
     :meal-event   {:type       action-type
                    :restaurant restaurant
                    :date       (get-today)}}))

(defmethod command-template->func [:in]
  [[[_ action-type]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :date   (get-today)}}))

(defmethod command-template->func [:out]
  [[[_ action-type]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :date   (get-today)}}))

(defmethod command-template->func [:order :food]
  [[[_ action-type] [_ food]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :food   food
                    :date   (get-today)}}))


(defn words->command-templates
  "returns all valid combinations of command elements for the given words"
  [words]
  (let [elements-per-word (map #(word->command-elements %) words)]
    (apply combo/cartesian-product elements-per-word)))


(defn text->command-func
  [text]
  (let [words (str/split text #" +")]
    (->> words
         (words->command-templates)
         (map remove-filler)
         (map merge-food-elements)
         (some command-template->func))))


(defn message->command
  "parses the message text and returns a command map"
  [user-id text]
  (let [cmd-func (text->command-func text)]
    (if cmd-func
      (cmd-func user-id)
      {:command-type :unrecognized})))