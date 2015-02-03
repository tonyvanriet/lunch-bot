(ns lunch-bot.comm
  (:require [clojure.string :as str]
            [clj-slack-client
             [team-state :as state]
             [rtm-transmit :as tx]]))


;;
;; Parsing user commands
;;
;; the input string is broken into words.
;; each word is classified as a particular kind of command element.
;; command elements can be:
;;   specific keywords (:paid, :bought, :cost, :show, :balances)
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
  (when-let [[_ user-id] (re-find #"<@(U.+)>" word)]
    user-id))


(def keyword-strs ["paid" "bought" "cost" "show" "balances"])


(defn word->keyword
  [word]
  (let [lword (.toLowerCase word)]
    (when-let [keyword-str (some #(when (.startsWith % lword) %) keyword-strs)]
      (keyword keyword-str))))


(defn word->amount
  [word]
  ;; regex is a modified version of this answer on SO
  ;; http://stackoverflow.com/questions/354044/what-is-the-best-u-s-currency-regex
  (when-let [[_ amount-str] (re-find #"^[+-]?\$?(([0-9]{1,3}(?:,?[0-9]{3})*|[0-9]*)(?:\.[0-9]{1,2})?)$" word)]
    (-> amount-str
        (.replaceAll "," "")
        (Double.))))


(defn word->command-element
  [word]
  (let [keyword (word->keyword word)]
    (cond keyword keyword
          (word->user-id word) :user
          (word->amount word) :amount)))


(defn command-template->func
  [command-template]
  (cond (= command-template [:paid :user :amount])
        (fn [words commander]
          {:person    commander
           :type      :paid
           :amount    (word->amount (nth words 2))
           :recipient (word->user-id (nth words 1))})))


(defn parse-command
  [text]
  (let [words (str/split text #" +")
        command-template (map #(word->command-element %) words)]
    (when-let [cmd-func (command-template->func command-template)]
      (fn [commander] (cmd-func words commander)))))


(defn message->command-text
  [channel-id text]
  (if (state/dm? channel-id)
    text
    (when-let [[_ cmd-text] (re-find #"lunch (.*)" text)]
      cmd-text)))


(defn message->command-func
  [channel-id text]
  (when-let [cmd-text (message->command-text channel-id text)]
    (parse-command cmd-text)))


(defn process-command
  "parses the message text for a command, carries out that command,
  and returns a reply string"
  [channel-id user-id text]
  (let [cmd-func (message->command-func channel-id text)]
    (if cmd-func
      (str (cmd-func user-id))
      "huh?")))


(defn handle-message
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-reply (process-command channel-id user-id text)]
      (tx/say-message channel-id cmd-reply))))

