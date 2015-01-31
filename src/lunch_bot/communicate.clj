(ns lunch-bot.communicate
  (:require [clj-slack-client
             [team-state :as state]
             [rtm-transmit :as tx]]))


(defn parse-command
  [text]
  (str text "?"))


(defn message->command-text
  [channel-id text]
  (if (state/dm? channel-id)
    text
    (when-let [[_ cmd-text] (re-find #"lunch (.*)" text)]
      cmd-text)))


(defn message->command
  [channel-id text]
  (when-let [cmd-text (message->command-text channel-id text)]
    (parse-command cmd-text)))


(defn handle-message
  [{text :text, user-id :user, channel-id :channel}]
  (let [user (state/id->user user-id)]
    (when (not (:is_bot user))
      (when-let [cmd (message->command channel-id text)]
        (tx/say-message channel-id cmd)))))
