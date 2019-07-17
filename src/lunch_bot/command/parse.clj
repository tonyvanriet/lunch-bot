(ns lunch-bot.command.parse
  (require
    [clj-time.core :as time]
    [clj-time.format :refer [formatter parse]]
    [clj-time.coerce :refer [to-local-date]]
    [clj-slack-client.team-state :as team]
    [clojure.string :as str]
    [lunch-bot.store :as store]
    [lunch-bot.restaurant :as restaurant]))


(def restaurants-filename "restaurants.edn")

(defn text->words
  [text]
  (-> text
      (str/replace "\n" "\n ")
      (str/split #" +")))

(def action-str-map {:show     ["show"]
                     :paid     ["paid"]
                     :borrowed ["borrowed " "owe"]
                     :bought   ["bought"]
                     :cost     ["cost"]
                     :choose   ["choose"]
                     :order    ["order"]
                     :reorder  ["reorder"]
                     :in       ["in"]
                     :out      ["out"]
                     :help     ["help"]
                     :+tax     ["tax" "+tax"]
                     :nag      ["nags"]
                     :suggest  ["suggest"]
                     :diners   ["diners" "people"]})

(def noun-str-map {:balances      ["balances"]
                   :pay?          ["pay?"]
                   :history       ["history"]
                   :ordered?      ["ordered?"]
                   :discrepancies ["discrepancy" "discrepancies"]})

(def filler-str-map {:filler ["lunch" "for" "i" "my" "i'm" "on" "have"
                              "the" "what" "who" "should" "+" "plus" "to" "from"]})

(def relative-date-str-map {:today     ["today"]
                            :yesterday ["yesterday"]})


(defn word-match
  [word comp]
  (if (str/blank? word)
    (= word comp)
    (.startsWith comp word)))

(defn first-word-match
  [word strs]
  (some #(when (word-match word %) %) strs))

(defn word->element-keyword
  "finds the first str in the str-map that starts with the given word
  and returns the associated key. assumes the word is lower-case."
  [word str-map]
  (some->> str-map
           (filter #(first-word-match word (val %)))
           (first)
           (key)))

(defn word->action [word] (word->element-keyword word action-str-map))

(defn word->noun [word] (word->element-keyword word noun-str-map))

(defn word->filler [word] (word->element-keyword word filler-str-map))

(defn word->relative-date [word] (word->element-keyword word relative-date-str-map))


(defn word->user-id
  [word]
  (if-let [[_ user-id] (re-find #"<@(U\w+)(?:\|\w+)?>" word)]
    user-id
    (team/name->id word)))

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

(defn relative-date->date
  [relative-date]
  (case relative-date
    :today (time/today)
    :yesterday (time/minus (time/today) (time/days 1))      ; time/yesterday would return (now - 1 day) as a DateTime
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


(defn normalize-restaurant-word
  [word] (-> word
             (str/lower-case)
             (str/replace #"\W" "")))

(defn word->restaurants
  "returns the restaurants for which a word in the name starts with the given word"
  [word]
  (let [norm-word (normalize-restaurant-word word)]
    (filter (fn [w] (->> (:name w)
                         (text->words)
                         (map normalize-restaurant-word)
                         (some #(.startsWith % norm-word))))
            (restaurant/get-restaurants))))

