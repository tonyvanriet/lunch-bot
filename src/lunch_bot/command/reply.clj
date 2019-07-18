(ns lunch-bot.command.reply
  (:require
    [lunch-bot.talk :as talk]
    [lunch-bot.money :as money]
    [lunch-bot.meal :as meal]
    [clj-time.core :as time]))


(defn make-command-return-reply
  "build a reply to be distributed to the channel that the command was received on"
  [cmd reply-text]
  (talk/make-channel-message (:channel-id cmd) reply-text))


(defn events->reply
  [events]
  (when (seq events)
    (->> events
         (map talk/event->reply-str)
         (interpose "\n")
         (apply str))))


(defn dispatch-command->replies [cmd aggs events] ((juxt :command-type :info-type) cmd))

(defmulti command->replies
          "formulates the replies to be returned for the command, if any."
          #'dispatch-command->replies)


(defmethod command->replies :default [_ _ _] nil)

(defmethod command->replies [:unrecognized nil]
  [cmd _ _]
  [(make-command-return-reply cmd "huh?")])

(defmethod command->replies [:help nil]
  [cmd _ _]
  [(talk/make-user-message (:requestor cmd) (slurp "help.md"))])

(defmethod command->replies [:show :balances]
  [cmd {:keys [balances] :as aggs} _]
  (->> balances
       (money/sort-balances)
       (reverse)
       (talk/balances->str)
       (make-command-return-reply cmd)
       (vector)))

(defmethod command->replies [:show :pay?]
  [{:keys [requestor] :as cmd} {:keys [balances] :as aggs} _]
  [(talk/make-user-message requestor (if-let [payment (money/best-payment requestor balances)]
                                       (talk/event->str payment)
                                       (str "Keep your money.")))])

#_(defmethod command->replies [:show :payoffs]
  [cmd {:keys [balances] :as aggs} _]
  (->> balances
       (money/minimal-payoffs)
       (talk/payoffs->str)
       (make-command-return-reply cmd)
       (vector)))

(defmethod command->replies [:show :history]
  [cmd {:keys [money-events] :as aggs} _]
  (->> money-events
       (talk/recent-money-history)
       (make-command-return-reply cmd)
       (vector)))

(defmethod command->replies [:show :meal-summary]
  [{:keys [date] :as cmd} {:keys [meals] :as aggs} _]
  (let [meal (get meals date)]
    (cond (time/after? date (time/today)) [(make-command-return-reply cmd (str "That lunch hasn't happened yet."))]
          :else [(make-command-return-reply cmd (if (or (time/before? date (time/today)) (meal/any-bought? meal))
                                            (talk/post-order-summary date meal)
                                            (talk/pre-order-summary meal)))])))

(defmethod command->replies [:show :ordered?]
  [{:keys [requestor] :as cmd} {:keys [meals] :as aggs} _]
  (let [todays-meal (get meals (time/today))
        reply-text (if-let [todays-restaurant (-> todays-meal :chosen-restaurant)]
                     (let [person-meals (meal/person-meal-history meals todays-restaurant requestor 3)]
                       (talk/person-meal-history person-meals todays-restaurant))
                     (str "Somebody needs to choose a restaurant first."))]
    [(talk/make-user-message requestor reply-text)]))

(defmethod command->replies [:show :discrepancies]
  [cmd {:keys [meals] :as aggs} _]
  (let [discrepant-meals (filter #(meal/is-discrepant (val %)) meals)]
    [(make-command-return-reply cmd (talk/discrepant-meals-summary discrepant-meals))]))

(defmethod command->replies [:submit-payment nil]
  [{:keys [amount to date] :as cmd} _ events]
  (cond (< amount 0M) [(make-command-return-reply
                         cmd (str "You can't pay a negative amount. Try using the borrowed command."))]
        (= amount 0M) [(make-command-return-reply
                         cmd (str "You paid " (talk/person->str to) " 0? Good for you."))]
        (time/after? date (time/today)) [(make-command-return-reply cmd (str "Don't talk to me about the future."))]
        :else (let [requestor-reply (make-command-return-reply cmd (events->reply events))
                    paid-event (first (filter #(= (:type %) :paid) events))
                    recipient-reply (talk/make-user-message (:to paid-event)
                                                            (talk/event->reply-str paid-event))]
                [requestor-reply recipient-reply])))

(defmethod command->replies [:submit-debt nil]
  [{:keys [amount from date] :as cmd} _ events]
  (cond (< amount 0M) [(make-command-return-reply
                         cmd (str "You can't borrow a negative amount. Try using the paid command."))]
        (= amount 0M) [(make-command-return-reply
                         cmd (str "You borrowed 0 from " (talk/person->str from) "? Good for you."))]
        (time/after? date (time/today)) [(make-command-return-reply cmd (str "Don't talk to me about the future."))]
        :else (let [requestor-reply (make-command-return-reply cmd (events->reply events))
                    borrowed-event (first (filter #(= (:type %) :borrowed) events))
                    recipient-reply (talk/make-user-message (:from borrowed-event)
                                                            (talk/event->reply-str borrowed-event))]
                [requestor-reply recipient-reply])))

(defmethod command->replies [:submit-bought nil]
  [{:keys [amount date] :as cmd} _ events]
  (cond (< amount 0M) [(make-command-return-reply cmd (str "You can't buy lunch for less than 0."))]
        (time/after? date (time/today)) [(make-command-return-reply cmd (str "Don't talk to me about the future."))]
        :else [(make-command-return-reply cmd (events->reply events))]))

(defmethod command->replies [:submit-cost nil]
  [{:keys [amount date] :as cmd} _ events]
  (cond (< amount 0M) [(make-command-return-reply cmd (str "Your lunch can't cost less than 0."))]
        (time/after? date (time/today)) [(make-command-return-reply cmd (str "Don't talk to me about the future."))]
        :else [(make-command-return-reply cmd (events->reply events))]))

(defmethod command->replies [:declare-in nil] [cmd _ events]
  [(make-command-return-reply cmd (events->reply events))])

(defmethod command->replies [:declare-out nil] [cmd _ events]
  [(make-command-return-reply cmd (events->reply events))])

(defmethod command->replies [:declare-diners nil]
  [{:keys [number]} _ _]
  [(talk/make-lunch-message (str number " people dining today."))])

(defmethod command->replies [:choose-restaurant nil] [_ _ events]
  [(talk/make-lunch-message (events->reply events))])

(defmethod command->replies [:suggest-restaurant nil]
  [{:keys [requestor restaurant]} _ _]
  [(talk/make-lunch-message (str (talk/person->str requestor) " thinks we should eat " (:name restaurant)))])

(defmethod command->replies [:submit-order nil] [cmd _ events]
  [(make-command-return-reply cmd (events->reply events))])

(defmethod command->replies [:copy-order nil]
  [{:keys [requestor copy-from] :as cmd} _ events]
  (if (= events [])
    [(talk/make-user-message
       requestor
       (str "Could not find " copy-from "'s meal"))]
    [(make-command-return-reply cmd (events->reply events))]))

(defmethod command->replies [:reorder nil]
  [{:keys [requestor index] :as cmd} {:keys [meals] :as aggs} events]
  (if (> (count events) 0)
    [(make-command-return-reply cmd (events->reply events))]
    ; if we didn't emit an event, either there is no restaurant or there isn't
    ; a meal at index `index`
    (let [todays-meal (get meals (time/today))
          reply-text (if (nil? (:chosen-restaurant todays-meal))
                       (str "Couldn't find meal " (+ index 1) " in your recent meals")
                       "Somebody needs to choose a restaurant first.")]
      [(talk/make-user-message requestor reply-text)])))
