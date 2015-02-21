(ns lunch-bot.command.parse
  (require
    [clj-time.core :as tc]
    [clj-time.format :as tf]
    [clj-slack-client.team-state :as team]
    [clojure.string :as str]))


(defn text->words
  [text]
  (-> text
      (str/replace "\n" "\n ")
      (str/split #" +")))


(def action-strs ["paid" "bought" "cost" "undo" "restaurant"
                  "want" "choose" "order" "in" "out"])

(def noun-strs ["balances" "pay?" "payoffs" "history" "today"])

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
  (if-let [[_ user-id] (re-find #"<@(U\w+)(?:\|\w+)?>" word)]
    user-id
    (team/name->id word)))

(defn word->noun
  [word]
  (word->keyword word noun-strs))

(defn word->filler
  [word]
  (-> word
      (.toLowerCase)
      (first-starts-with filler-strs)))

(def amount-regex #"^((?:[+-]?\$?(?:[0-9]{1,3}(?:,?[0-9]{3})*|[0-9]+)(?:\.[0-9]{1,2})?)|(?:\.[0-9]{1,2}))$")

(defn word->amount
  [word]
  ;; regex is a modified version of this answer on SO
  ;; http://stackoverflow.com/questions/354044/what-is-the-best-u-s-currency-regex
  (when-let [[_ amount-str]
             (re-find amount-regex word)]
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
