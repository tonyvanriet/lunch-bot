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

(def ^:dynamic *api-token* nil)


(defn get-lunch-channel-id [] (:id (state/name->channel lunch-channel-name)))


(defn get-user-dm-id
  "get the direct message channel id for this user.
  open the dm channel if it hasn't been opened yet."
  [user-id]
  (if-let [dm-id (state/user-id->dm-id user-id)]
    dm-id
    (web/im-open *api-token* user-id)))


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
  [event _]
  (let [restaurant (:restaurant event)
        menu-url (:menu-url restaurant)
        channel-id (get-lunch-channel-id)]
    (web/channels-setTopic *api-token* channel-id
                           (str "ordering " (:name restaurant)
                                (when menu-url (str " " menu-url))))))

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

(defn heartbeat []
  (handle-command {:command-type :find-nags
                   :date         (time/today)}))

(defn start-heartbeat []
  (swap! heartbeating (constantly true))
  (future
    (loop []
      (Thread/sleep 5000)
      (heartbeat)
      (when @heartbeating (recur)))))

(defn stop-heartbeat []
  (swap! heartbeating (constantly false)))


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
  ([]
   (start (store/read-api-token api-token-filename)))
  ([api-token]
   (try
     (event/initialize-events)
     (restaurant/initialize-restaurants)
     (alter-var-root (var *api-token*) (constantly api-token))
     (slack/connect *api-token* try-handle-slack-event)
     (start-heartbeat)
     (println "lunch-bot running...")
     (catch Exception ex
       (println ex)
       (println "couldn't start lunch-bot")
       (stop)))))

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


; todo ‘order usual’ - usual defaults to most recent order, or user setting
; todo 'usual food food food' - set usual for chosen restaurant
; todo 'yesterday', 'monday', 'last thursday', 'this week', '1/28'
; todo 'add superdawg' or 'restaurant superdawg' - create restaurant
; todo 'add superdawg http://www.superdawg.com/menu.cfm 773-763-0660'
; todo 'superdawg 7737630660 http://www.superdawg.com/menu.cfm' - update restaurant
; todo 'bw3 thursday'
; todo 'remove superdog', 'rename superdog superdawg'
; todo recognize 'steve paid carla 23' for privileged users
; todo talk converts person's name to "you" in DMs
; todo attempt to interpret multi-line messages as one command per line
; todo lunchbot suggests a restaurant based on history for day of week, or at random
; todo handle edited messages
