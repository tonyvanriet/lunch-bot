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
;; the command template is compared against a list of recognized command
;; templates.
;; if a match is found, that command template is used to create a command
;; map with the parsed data from the template (user-ids, amounts, dates).
;; that command map is then returned to the caller to be used to perform
;; the corresponding action.
;; if no match is found, it's considered an invalid command.
;;   could recognize partial command templates give the user an error
;;   message indicating the unrecognized parts.
;;


(defn get-command-signature-re []
  "returns a regex that will match a command signature, indicating
  that the user wants lunchbot to interpret the message as a command"
  (let [linkified-self (tx/linkify (state/self-id))]
    (re-pattern (str linkified-self ":?"))))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (when text
    (let [cmd-signature-re (get-command-signature-re)
          has-cmd-signature (re-find cmd-signature-re text)]
      (when (or (state/dm? channel-id) has-cmd-signature)
        (-> text
            (str/replace cmd-signature-re "")
            (str/trim))))))


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


(defn merge-elements
  "reduces the elements in the command-template, aggregating the next element into
  the previous if the merge function returns a merged element"
  [command-template merge]
  (reduce (fn [template element]
            (if-let [merged-elem (merge (last template) element)]
              (conj (vec (butlast template)) merged-elem)
              (conj template element)))
          []
          command-template))

(defn merge-food-elements
  [command-template]
  (merge-elements command-template (fn [[k1 v1] [k2 v2]]
                                     (when (= :food k1 k2)
                                       [:food (str v1 (when (not (= (last v1) \newline)) " ") v2)]))))

(defn merge-restaurant-elements
  [command-template]
  (merge-elements command-template (fn [[k1 v1] [k2 v2]]
                                     (when (and (= :restaurant k1 k2)
                                                (= v1 v2))
                                       [:restaurant v1]))))


(defn words->command-templates
  "returns all valid combinations of command elements for the given words"
  [words]
  (let [elements-per-word (map #(word->command-elements %) words)]
    (apply combo/cartesian-product elements-per-word)))


(defn command-text->command
  "parses the command text and returns a command map"
  [text]
  (let [templates (->> (parse/text->words text)
                       (words->command-templates)
                       (map remove-filler)
                       (map merge-food-elements)
                       (map merge-restaurant-elements))
        cmd (some template/command-template->command templates)]
    (if cmd cmd {:command-type :unrecognized})))

