(ns lunch-bot.command
  (:require
    [lunch-bot.command.parse :as parse]
    [lunch-bot.command.template :as template]
    [clj-slack-client.rtm-transmit :as tx]
    [clj-slack-client.team-state :as state]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]))

;;
;; Parsing user commands
;;
;; the input string is broken into words.
;; each word is classified as a particular kind of command element.
;; command elements can be:
;;   specific keywords (:paid, :bought, :cost, :balances, :pay?, :history)
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


(defn get-channel-command-signature []
  "generates a regex that will match against a channel message that should
  be interpreted as a command, and captures the command text from the message."
  (let [linkified-self (tx/linkify (state/self-id))]
    (str linkified-self ":? *(.*)")))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (when text
    (if (state/dm? channel-id)
      text
      (when-let [[_ cmd-text] (-> (get-channel-command-signature)
                                  (re-pattern)
                                  (re-find text))]
        cmd-text))))


(defn word->command-elements
  "runs the word through all element parsers and returns all
  elements with non-nil values"
  [word]
  (let [trimmed-word (str/trim word)
        action-keyword (parse/word->action trimmed-word)
        elements [[action-keyword action-keyword]
                  [:noun (parse/word->noun trimmed-word)]
                  [:user (parse/word->user-id trimmed-word)]
                  [:amount (parse/word->amount trimmed-word)]
                  [:date (parse/word->date trimmed-word)]
                  [:restaurant (parse/word->restaurant trimmed-word)]
                  [:food (parse/word->food word)]
                  [:filler (parse/word->filler trimmed-word)]]]
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


(defn words->command-templates
  "returns all valid combinations of command elements for the given words"
  [words]
  (let [elements-per-word (map #(word->command-elements %) words)]
    (apply combo/cartesian-product elements-per-word)))


(defn text->command-func
  [text]
  (->> (parse/text->words text)
       (words->command-templates)
       (map remove-filler)
       (map merge-food-elements)
       (some template/command-template->func)))


(defn message->command
  "parses the message text and returns a command map"
  [user-id text]
  (let [cmd-func (text->command-func text)]
    (if cmd-func
      (cmd-func user-id)
      {:command-type :unrecognized})))