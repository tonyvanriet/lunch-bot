(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot
     [comm :as comm]
     [money :as money]]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [rtm-transmit :as tx]]
    [clojure.pprint :refer [pprint]]
    [clojure.java.io :as io]
    [clojure.string :as string]))


(def api-token-filename "api-token.txt")

(defn get-api-token-from-file
  [filename]
  (let [file (io/file filename)]
    (if (.exists file)
      (string/trim (slurp file))
      (do (spit filename "your-api-token")
          (println "put your api token in" filename)))))


(def ^:private money-events (atom nil))

(defn get-money-events-from-file
  [filename]
  (let [file (io/file filename)]
    (when (and (.exists file)
               (< 0 (.length file)))
      (read-string (slurp file)))))

(def money-events-filename "events.txt")

(defn initialize-money-events []
  (swap! money-events (fn [_] (get-money-events-from-file money-events-filename))))


(defn message->command-text
  "determines if the message should be interpreted as a command, and if so, returns
  the command text from the message."
  [channel-id text]
  (if (state/dm? channel-id)
    text
    (let [linkified-self (tx/linkify (state/self-id))]
      (when-let [[_ cmd-text] (re-find (re-pattern (str linkified-self ":? *(.*)")) text)]
        cmd-text))))


(defn pstr
  [object]
  (with-out-str (pprint object)))


(defn balances->str [balances]
  (pstr balances))

(defn payoffs->str [payoffs]
  (pstr payoffs))

(defn events->str [events]
  (pstr events))


(defmulti handle-command :command-type)

(defmethod handle-command :unrecognized
  [_]
  "huh?")

(defmethod handle-command :show
  [{:keys [info-type]}]
  (cond (= info-type :balances)
        (->> @money-events
             (money/events->balances)
             (balances->str))

        (= info-type :payoffs)
        (->> @money-events
             (money/events->balances)
             (money/minimal-payoffs)
             (payoffs->str))

        (= info-type :events)
        (events->str @money-events)))

(defmethod handle-command :event
  [{:keys [event]}]
  (swap! money-events (fn [events] (conj events event)))
  (spit "events.txt" (pprint @money-events))
  (str "added event " event))


(defmulti handle-slack-event :type)

(defmethod handle-slack-event "message"
  [{channel-id :channel, user-id :user, text :text}]
  (when (not (state/bot? user-id))
    (when-let [cmd-text (message->command-text channel-id text)]
      (let [cmd (comm/message->command user-id cmd-text)
            cmd-reply (handle-command cmd)]
        (when cmd-reply
          (tx/say-message channel-id cmd-reply))))))

(defmethod handle-slack-event "channel_joined" [event]
  nil)

(defmethod handle-slack-event :default [event]
  nil)


(defn wait-for-console-quit []
  (loop []
    (let [input (read-line)]
      (when-not (= input "q")
        (recur)))))


(defn shutdown-app []
  (slack/disconnect)
  (println "...lunch-bot dying"))


(defn start
  ([]
    (start (get-api-token-from-file api-token-filename)))
  ([api-token]
    (initialize-money-events)
    (slack/connect api-token handle-slack-event)
    (println "lunch-bot running...")))

(defn stop []
  (shutdown-app))

(defn restart []
  (stop)
  (start))


(defn -main
  [& args]
  (try
    (start)
    (wait-for-console-quit)
    (finally
      (stop)
      (shutdown-agents))))

