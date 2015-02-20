(ns lunch-bot.meal)


;;
;; a meal contains the set of people that are in for lunch, the set that
;; are out, and a collection of restaurants that have been chosen along
;; with order info for each restaurant.
;;


(def example-meal
  {:chosen-restaurant {:name "BW3"}
   :people            {"bob"  {:status :in
                               :order  "jumbo dog\nno hot peppers\nlg fry"
                               :cost   8.5M}
                       "rozz" {:status :in
                               :order  "bowl of chili"
                               :cost   9M}
                       "biff" {:status :out}}})


(defmulti apply-event-to-meal #(:type %2))

(defmethod apply-event-to-meal :want
  [meal {:keys [person restaurant] :as event}]
  (assoc-in meal [:people person :wants] restaurant))

(defmethod apply-event-to-meal :in
  [meal {:keys [person] :as event}]
  (assoc-in meal [:people person :status] :in))

(defmethod apply-event-to-meal :out
  [meal {:keys [person] :as event}]
  (assoc-in meal [:people person :status] :out))

(defmethod apply-event-to-meal :choose
  [meal {:keys [restaurant] :as event}]
  (assoc-in meal [:chosen-restaurant] restaurant))

(defmethod apply-event-to-meal :order
  [meal {:keys [person food] :as event}]
  (assoc-in meal [:people person :order] food))

(defmethod apply-event-to-meal :bought
  [meal {:keys [person amount] :as event}]
  (update-in meal [:people person :bought-amount] (fnil + 0) amount))

(defmethod apply-event-to-meal :cost
  [meal {:keys [person amount] :as event}]
  (update-in meal [:people person :cost-amount] (fnil + 0) amount))

(defmethod apply-event-to-meal :default
  [meal _]
  meal)


(defn events->meal
  "constructs an aggregate view of the meal for the given events."
  [events]
  (reduce apply-event-to-meal {} events))

