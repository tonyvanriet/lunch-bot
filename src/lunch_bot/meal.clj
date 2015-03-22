(ns lunch-bot.meal
  (:require [clojure.core.incubator :refer [dissoc-in]]))


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
                               :cost   9M
                               :bought 20M}
                       "biff" {:status :out}}})


(defn dispatch-apply-event-to-meals [meals event] (:type event))

(defmulti apply-event-to-meals #'dispatch-apply-event-to-meals)

(defmethod apply-event-to-meals :want
  [meals {:keys [date person restaurant] :as event}]
  (assoc-in meals [date :people person :wants] restaurant))

(defmethod apply-event-to-meals :in
  [meals {:keys [date person] :as event}]
  (assoc-in meals [date :people person :status] :in))

(defmethod apply-event-to-meals :out
  [meals {:keys [date person] :as event}]
  (-> meals
      (assoc-in [date :people person :status] :out)
      (dissoc-in [date :people person :order])
      (dissoc-in [date :people person :cost])))

(defmethod apply-event-to-meals :choose
  [meals {:keys [date restaurant] :as event}]
  (assoc-in meals [date :chosen-restaurant] restaurant))

(defmethod apply-event-to-meals :order
  [meals {:keys [date person food] :as event}]
  (-> meals
      (assoc-in [date :people person :order] food)
      (assoc-in [date :people person :status] :in)))

(defmethod apply-event-to-meals :bought
  [meals {:keys [date person amount] :as event}]
  (update-in meals [date :people person :bought] (fnil + 0) amount))

(defmethod apply-event-to-meals :cost
  [meals {:keys [date person amount] :as event}]
  (-> meals
      (update-in [date :people person :cost] (fnil + 0) amount)
      (assoc-in [date :people person :status] :in)))

(defmethod apply-event-to-meals :default
  [meal _]
  meal)


(defn events->meals
  "constructs an aggregate view of the meal for each day."
  [events]
  (reduce apply-event-to-meals {} events))


(defn person-in? [meal person] (= (get-in meal [:people person :status]) :in))
(defn person-out? [meal person] (= (get-in meal [:people person :status]) :out))
(defn person-ordered? [meal person] (get-in meal [:people person :order])) ; good nil punning? or should I be using contains?
(defn person-bought? [meal person] (get-in meal [:people person :bought]))
(defn person-costed? [meal person] (get-in meal [:people person :cost]))

(defn people-in [meal] (filter #(person-in? meal %) (keys (:people meal))))
(defn people-out [meal] (filter #(person-out? meal %) (keys (:people meal))))
(defn people-bought [meal] (filter #(person-bought? meal %) (keys (:people meal))))

(defn any-bought? [meal] (some #(contains? % :bought) (vals (:people meal))))


(defn person-meal-history
  "returns the meal info for the last n times this person placed an order"
  [meals restaurant person n]
  (->> meals
       (filter #(= (:name restaurant) (-> (val %) :chosen-restaurant :name)))
       (filter #(person-ordered? (val %) person))
       (into (sorted-map))
       (reverse)
       (take n)
       (map #(-> % val :people (get person)))))

