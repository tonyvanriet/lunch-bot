(ns lunch-bot.command.template
  (:require [clj-time.core :as time]))


(defn command-template->element-keys
  [template]
  (map #(first %) template))


(defmulti command-template->func command-template->element-keys)

(defmethod command-template->func :default [_]
  nil)

(defmethod command-template->func [:noun]
  [[[_ noun]]]
  (fn [commander]
    {:command-type :show
     :info-type    noun
     :requestor    commander}))

(defmethod command-template->func [:paid :user :amount]
  [[[_ action-type] [_ user-id] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :to     user-id
                    :date   (time/today)}}))

(defmethod command-template->func [:paid :amount :user]
  [[action-elem amount-elem user-elem]]
  (command-template->func [action-elem user-elem amount-elem]))

(defmethod command-template->func [:bought :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :date   date}}))

(defmethod command-template->func [:bought :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->func [action-elem date-elem amount-elem]))

(defmethod command-template->func [:bought :amount]
  [[action-elem amount-elem]]
  (command-template->func [action-elem [:date (time/today)] amount-elem]))

(defmethod command-template->func [:cost :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  (fn [commander]
    {:command-type :event
     :event        {:person commander
                    :type   action-type
                    :amount amount
                    :date   date}}))

(defmethod command-template->func [:cost :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->func [action-elem date-elem amount-elem]))

(defmethod command-template->func [:cost :amount]
  [[action-elem amount-elem]]
  (command-template->func [action-elem [:date (time/today)] amount-elem]))

(defmethod command-template->func [:choose :restaurant]
  [[[_ action-type] [_ restaurant]]]
  (fn [_]
    {:command-type :meal-event
     :meal-event   {:type       action-type
                    :restaurant restaurant
                    :date       (time/today)}}))

(defmethod command-template->func [:in]
  [[[_ action-type]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :date   (time/today)}}))

(defmethod command-template->func [:out]
  [[[_ action-type]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :date   (time/today)}}))

(defmethod command-template->func [:order :food]
  [[[_ action-type] [_ food]]]
  (fn [commander]
    {:command-type :meal-event
     :meal-event   {:type   action-type
                    :person commander
                    :food   food
                    :date   (time/today)}}))
