(ns lunch-bot.communicate
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
;;   specific actions (:paid, :bought, :cost)
;;   the target of an action (:user, :restaurant)
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


(defn word->action
  [word]
  (let [lword (.toLowerCase word)]
    (cond (= lword "paid") :paid
          (= lword "bought") :bought
          (= lword "cost") :cost)))


(defn word->amount
  [word]
  (when-let [[_ amount] (re-find #"^\$?(\d*\.?\d{0,2})$" word)]
    (Double. amount)))


(defn word->command-element
  [word]
  (let [action (word->action word)]
    (cond action action
          (word->user-id word) :user
          (word->amount word) :amount)))


(defn command-template->func
  [command-template]
  (cond (= command-template [:paid :user :amount])
        (fn [words commander]
          {:person commander
           :type :paid
           :amount (word->amount (nth words 2))
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


(defn handle-message
  [{text :text, user-id :user, channel-id :channel}]
  (let [user (state/id->user user-id)]
    (when (not (:is_bot user))
      (let [cmd-func (message->command-func channel-id text)
            reply (if cmd-func
                    (str (cmd-func user-id))
                    "huh?")]
        (tx/say-message channel-id reply)))))

