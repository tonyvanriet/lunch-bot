(ns lunch-bot.core
  (:gen-class)
  (:require
    [lunch-bot
     [command :as command]
     [talk :as talk]
     [store :as store]
     [event :as event]
     [aggregate :as aggregate]
     [restaurant :as restaurant]]
    [lunch-bot.command.handler :as handler]
    [lunch-bot.command.reply :as reply]
    [clj-slack-client
     [core :as slack]
     [team-state :as state]
     [web :as web]]
    [clj-time.core :as time]))


(def api-token-filename "api-token.txt")
(def lunch-channel-name "lunch")
(def commands-filename "commands.edn")

(def api-token (store/read-api-token api-token-filename))

(defn get-lunch-channel-id [] (:id (state/name->channel lunch-channel-name)))


(defn get-user-dm-id
  "get the direct message channel id for this user.
  open the dm channel if it hasn't been opened yet."
  [user-id]
  (if-let [dm-id (state/user-id->dm-id user-id)]
    dm-id
    (web/im-open api-token user-id)))


(defn distribute-message
  [{distribution :distribution, text :text, :as msg}]
  (let [channel-id (case distribution
                     :channel (:channel-id msg)
                     :broadcast (get-lunch-channel-id)
                     :user (get-user-dm-id (:user-id msg)))]
    (talk/say-message channel-id text)))


(defn dispatch-handle-event [event aggs] (:type event))

(defmulti handle-event
          "performs side-effects for the event"
          #'dispatch-handle-event)

(defmethod handle-event :default [_ _] nil)

(defmethod handle-event :choose
  [{:keys [restaurant] :as event} _]
  (let [menu-url (:menu-url restaurant)]
    #_(web/channels-setTopic api-token (get-lunch-channel-id)
                           (str "ordering " (:name restaurant)
                                (when menu-url (str " " menu-url))))))

(defmethod handle-event :bought
  [{:keys [date] :as event} _]
  #_(when (= date (time/today))
    (web/channels-setTopic api-token (get-lunch-channel-id) "")))

(defmethod handle-event :found-nags
  [{:keys [date costless-ins boughtless?] :as event} {:keys [meals] :as aggs}]
  (let [meal (get meals date)
        post-order-summary (talk/post-order-summary date meal)
        cost-nag-messages (map #(talk/make-user-message % post-order-summary) costless-ins)
        bought-nag-message (talk/make-lunch-message (talk/bought-nag date))
        nag-messages (if boughtless?
                       (conj cost-nag-messages bought-nag-message)
                       cost-nag-messages)]
    (doseq [msg nag-messages]
      (distribute-message msg))))


(defn handle-command
  "translates the command into events, commits the events to the stream,
  handles the events, and returns replies."
  [cmd]
  (store/append-command cmd commands-filename)
  (let [aggs (aggregate/get-aggregates)
        events (handler/command->events cmd aggs)]
    (doseq [event events]
      (event/commit-event event))
    (let [updated-aggs (aggregate/get-aggregates)]
      (doseq [event events]
        (handle-event event updated-aggs))
      (reply/command->replies cmd updated-aggs events))))


(defn contextualize-command
  "apply slack message context to the raw command"
  [cmd {requestor :user, text :text, ts :ts, channel-id :channel, :as msg} cmd-text]
  (-> cmd
      (assoc :requestor requestor)
      (assoc :text text)
      (assoc :cmd-text cmd-text)
      (assoc :channel-id channel-id)
      (assoc :ts ts)))


(defn printex
  [msg ex]
  (let [line (apply str (repeat 100 "-"))]
    (println line)
    (println msg)
    (println ex)
    (clojure.stacktrace/print-stack-trace ex)
    (println line)))


(defn handle-message
  "translates a slack message into a command, handles that command, and communicates the reply"
  [{channel-id :channel, text :text, :as msg}]
  (try
    (when-let [cmd-text (command/message->command-text channel-id text)]
      (let [raw-cmd (command/command-text->command cmd-text)
            cmd (contextualize-command raw-cmd msg cmd-text)
            replies (handle-command cmd)]
        (doseq [reply replies]
          (distribute-message reply))))
    (catch Exception ex
      (printex (str "Exception trying to handle slack message\n" (str msg) ".") ex)
      (try (talk/say-message channel-id "@!#?@!")))))


(defn dispatch-handle-slack-event [event] ((juxt :type :subtype) event))

(defmulti handle-slack-event #'dispatch-handle-slack-event)

(defmethod handle-slack-event ["message" nil]
  [{user-id :user, :as msg}]
  (when (not (state/bot? user-id))
    (handle-message msg)))

(defmethod handle-slack-event ["message" "message_changed"]
  [{channel-id :channel, {text :text} :message}]
  (when (command/message->command-text channel-id text)
    (talk/say-message channel-id "huh?")))

(defmethod handle-slack-event ["channel_joined" nil]
  [event]
  nil)

(defmethod handle-slack-event :default
  [event]
  nil)


(defn try-handle-slack-event
  [event]
  (try
    (handle-slack-event event)
    (catch Exception ex
      (printex (str "Exception trying to handle slack event\n" (str event) ".") ex))))


(def heartbeating (atom false))

(def heartbeat-loop (atom nil))

(defn heartbeat []
  #_(handle-command {:command-type :find-nags
                   :date         (time/today)
                   :ts           (slack/time->ts (time/now))}))


(defn start-heartbeat []
  (swap! heartbeating (constantly true))
  (swap! heartbeat-loop (constantly
                          (future
                            (loop []
                              (heartbeat)
                              (Thread/sleep 5000)
                              (when @heartbeating (recur)))))))

(defn stop-heartbeat []
  "kill the heartbeat loop and block until the loop exits"
  (swap! heartbeating (constantly false))
  (future-cancel @heartbeat-loop))


(defn wait-for-console-quit []
  (loop []
    (let [input (read-line)]
      (when-not (= input "q")
        (recur)))))


(defn shutdown-app []
  ()
  (stop-heartbeat)
  (slack/disconnect)
  (println "...lunch-bot dying"))


(defn stop []
  (shutdown-app))

(defn start
  []
  (try
    (event/initialize-events)
    (restaurant/initialize-restaurants)
    (slack/connect api-token try-handle-slack-event)
    (start-heartbeat)
    (println "lunch-bot running...")
    (catch Exception ex
      (println ex)
      (println "couldn't start lunch-bot")
      (stop))))

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
