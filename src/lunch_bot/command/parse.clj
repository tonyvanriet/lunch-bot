(ns lunch-bot.command.parse
  (require
    [clj-time.core :as time]
    [clj-time.format :refer [formatter parse]]
    [clj-time.coerce :refer [to-local-date]]
    [clj-slack-client.team-state :as team]
    [clojure.string :as str]))


(defn text->words
  [text]
  (-> text
      (str/replace "\n" "\n ")
      (str/split #" +")))


(def action-strs ["show" "paid" "bought" "cost" "undo"
                  "want" "choose" "order" "in" "out"])

(def noun-strs ["balances" "pay?" "payoffs" "history" "ordered?"])

(def filler-strs ["lunch" "for" "i" "my" "i'm"])

(def relative-date-strs ["today" "yesterday"])

; todo refactor these recognized strs to be a seq of strs that map to a keyword
; to allow different user input to map to the same element


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
  (formatter (time/default-time-zone) "YYYYMMdd" "YYYY-MM-dd" "YYYY/MM/dd"))

(defn word->relative-date
  [word]
  (word->keyword word relative-date-strs))

(defn relative-date->date
  [relative-date]
  (case relative-date
    :today (time/today)
    :yesterday (time/minus (time/today) (time/days 1))            ; tc/yesterday returns (now - 1 day) as a DateTime
    :default nil))

(defn word->date
  [word]
  (if-let [relative-date (word->relative-date word)]
    (relative-date->date relative-date)
    (try
      (to-local-date (parse date-formatter word))
      (catch IllegalArgumentException ex
        nil))))

(defn word->food
  [word]
  word)


(def ^:private restaurants (atom []))
(swap! restaurants
       (fn [_] (map #(hash-map :name %) ["BW3" "Chipotle" "Portillo's" "Elephant" "Smoque"
                                          "Superdawg" "Naf Naf" "Makisu" "Jimmy John's" "Potbelly"])))

; todo handle multi-word restaurant names

(defn normalize-restaurant-word
  [word] (-> word
             (.toLowerCase)
             (str/replace #"\W" "")))

(defn word->restaurant
  "returns the first restaurant for which the name starts with the word"
  [word]
  (let [norm-word (normalize-restaurant-word word)]
    (first (filter #(-> (:name %)
                        (normalize-restaurant-word)
                        (.startsWith norm-word))
                   @restaurants))))
