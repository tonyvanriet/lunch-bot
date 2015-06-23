(ns lunch-bot.command.reply
  (:require
    [lunch-bot.talk :as talk]
    [lunch-bot.money :as money]
    [lunch-bot.meal :as meal]
    [clj-time.core :as time]))


(defn make-channel-reply
  "build a reply to be distributed to a particular channel"
  [channel-id reply-text]
  {:distribution :channel, :channel-id channel-id, :text reply-text})

(defn make-broadcast-reply
  "build a reply to be distributed to the designated lunch broadcast channel"
  [reply-text]
  {:distribution :broadcast, :text reply-text})

(defn make-user-reply
  "build a reply to be distributed to an individual user"
  [user-id reply-text]
  {:distribution :user, :user-id user-id, :text reply-text})


(defn standard-replies
  "builds a list of replies in a way that is common to most of the command types.
  a single reply is returned, directed at the channel the command came in on."
  [cmd reply-text]
  [(make-channel-reply (:channel-id cmd) reply-text)])


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
  (standard-replies cmd "huh?"))

(defmethod command->replies [:help nil]
  [cmd _ _]
  [(make-user-reply (:requestor cmd) (slurp "help.md"))])

(defmethod command->replies [:show :balances]
  [cmd {:keys [balances] :as aggs} _]
  (->> balances
       (money/sort-balances)
       (reverse)
       (talk/balances->str)
       (standard-replies cmd)))

(defmethod command->replies [:show :pay?]
  [{:keys [requestor] :as cmd} {:keys [balances] :as aggs} _]
  [(make-user-reply requestor (if-let [payment (money/best-payment requestor balances)]
                                (talk/event->str payment)
                                (str "Keep your money.")))])

(defmethod command->replies [:show :payoffs]
  [cmd {:keys [balances] :as aggs} _]
  (->> balances
       (money/minimal-payoffs)
       (talk/payoffs->str)
       (standard-replies cmd)))

(defmethod command->replies [:show :history]
  [cmd {:keys [money-events] :as aggs} _]
  (->> money-events
       (talk/recent-money-history)
       (standard-replies cmd)))

(defmethod command->replies [:show :meal-summary]
  [{:keys [date] :as cmd} {:keys [meals] :as aggs} _]
  (let [meal (get meals date)]
    (standard-replies cmd (if (or (time/before? date (time/today)) (meal/any-bought? meal))
                            (talk/post-order-summary date meal)
                            (talk/pre-order-summary meal)))))

(defmethod command->replies [:show :ordered?]
  [{:keys [requestor] :as cmd} {:keys [meals] :as aggs} _]
  (let [todays-meal (get meals (time/today))
        reply-text (if-let [todays-restaurant (-> todays-meal :chosen-restaurant)]
                     (let [person-meals (meal/person-meal-history meals todays-restaurant requestor 3)]
                       (talk/person-meal-history person-meals todays-restaurant))
                     (str "Somebody needs to choose a restaurant first."))]
    [(make-user-reply requestor reply-text)]))

(defmethod command->replies [:show :discrepancies]
  [cmd {:keys [meals] :as aggs} _]
  (let [discrepant-meals (filter #(meal/is-discrepant (val %)) meals)]
    (standard-replies cmd (talk/discrepant-meals-summary discrepant-meals))))

(defmethod command->replies [:submit-payment nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:submit-bought nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:submit-cost nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:declare-in nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:declare-out nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:choose-restaurant nil] [cmd _ events] (standard-replies cmd (events->reply events)))
(defmethod command->replies [:submit-order nil] [cmd _ events] (standard-replies cmd (events->reply events)))

(defn bought-nag-str
  [date]
  (str "If you bought lunch" (when (not= date (time/today)) (str " on " date)) ", let me know."))

(defmethod command->replies [:send-nags nil]
  [{:keys [date] :as cmd} {:keys [meals] :as aggs} _]
  (let [meal (get meals date)
        meal-summary (meal/summary meal)
        costless-ins (:costless-ins meal-summary)
        cost-replies (vec (map #(make-user-reply % (talk/post-order-summary date meal)) costless-ins))]
    (if (meal/any-bought? meal)
      cost-replies
      (conj cost-replies (make-broadcast-reply (bought-nag-str date))))))

