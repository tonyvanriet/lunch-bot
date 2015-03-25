(ns lunch-bot.command.template
  (:require [clj-time.core :as time]))


(defn command-template->element-keys
  [template]
  (map #(first %) template))


(defmulti command-template->command #'command-template->element-keys)

(defmethod command-template->command :default [_]
  nil)

(defmethod command-template->command [:show :noun]
  [[[_ action-type] [_ noun]]]
  {:command-type action-type
   :info-type    noun})

(defmethod command-template->command [:noun]
  [[noun-elem]]
  (command-template->command [[:show :show] noun-elem]))

(defmethod command-template->command [:show :date]
  [[[_ action-type] [_ date]]]
  {:command-type action-type
   :info-type    :meal-summary
   :date         date})

(defmethod command-template->command [:date]
  [[date-elem]]
  (command-template->command [[:show :show] date-elem]))

(defmethod command-template->command [:paid :user :amount]
  [[[_ action-type] [_ user-id] [_ amount]]]
  {:command-type :event
   :event        {:type   action-type
                  :amount amount
                  :to     user-id
                  :date   (time/today)}})

(defmethod command-template->command [:paid :amount :user]
  [[action-elem amount-elem user-elem]]
  (command-template->command [action-elem user-elem amount-elem]))

(defmethod command-template->command [:bought :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  {:command-type :event
   :event        {:type   action-type
                  :amount amount
                  :date   date}})

(defmethod command-template->command [:bought :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->command [action-elem date-elem amount-elem]))

(defmethod command-template->command [:bought :amount]
  [[action-elem amount-elem]]
  (command-template->command [action-elem [:date (time/today)] amount-elem]))

(defmethod command-template->command [:cost :date :amount]
  [[[_ action-type] [_ date] [_ amount]]]
  {:command-type :event
   :event        {:type   action-type
                  :amount amount
                  :date   date}})

(defmethod command-template->command [:cost :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->command [action-elem date-elem amount-elem]))

(defmethod command-template->command [:cost :amount]
  [[action-elem amount-elem]]
  (command-template->command [action-elem [:date (time/today)] amount-elem]))


(defn command-meal-event
  [action-type]
  {:command-type :meal-event
   :meal-event   {:type action-type
                  :date (time/today)}})

(defn command-meal-event-restaurant
  [action-type restaurant]
  {:command-type :meal-event
   :meal-event   {:type       action-type
                  :restaurant restaurant
                  :date       (time/today)}})

(defn command-meal-event-food
  [action-type food]
  {:command-type :meal-event
   :meal-event   {:type action-type
                  :food food
                  :date (time/today)}})

(defmethod command-template->command [:in]
  [[[_ action-type]]]
  (command-meal-event action-type))

(defmethod command-template->command [:out]
  [[[_ action-type]]]
  (command-meal-event action-type))

(defmethod command-template->command [:want :restaurant]
  [[[_ action-type] [_ restaurant]]]
  (command-meal-event-restaurant action-type restaurant))

(defmethod command-template->command [:choose :restaurant]
  [[[_ action-type] [_ restaurant]]]
  (command-meal-event-restaurant action-type restaurant))

(defmethod command-template->command [:order :food]
  [[[_ action-type] [_ food]]]
  (command-meal-event-food action-type food))
