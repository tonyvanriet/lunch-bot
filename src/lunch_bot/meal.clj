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


(defmulti apply-event-to-meals #(:type %2))

(defmethod apply-event-to-meals :want
  [meal {:keys [date person restaurant] :as event}]
  (assoc-in meal [date :people person :wants] restaurant))

(defmethod apply-event-to-meals :in
  [meal {:keys [date person] :as event}]
  (assoc-in meal [date :people person :status] :in))

(defmethod apply-event-to-meals :out
  [meal {:keys [date person] :as event}]
  (assoc-in meal [date :people person :status] :out))

(defmethod apply-event-to-meals :choose
  [meal {:keys [date restaurant] :as event}]
  (assoc-in meal [date :chosen-restaurant] restaurant))

(defmethod apply-event-to-meals :order
  [meal {:keys [date person food] :as event}]
  (assoc-in meal [date :people person :order] food))

(defmethod apply-event-to-meals :bought
  [meal {:keys [date person amount] :as event}]
  (update-in meal [date :people person :bought-amount] (fnil + 0) amount))

(defmethod apply-event-to-meals :cost
  [meal {:keys [date person amount] :as event}]
  (update-in meal [date :people person :cost-amount] (fnil + 0) amount))

(defmethod apply-event-to-meals :default
  [meal _]
  meal)


(defn events->meals
  "constructs an aggregate view of the meal for each day."
  [events]
  (reduce apply-event-to-meals {} events))


(defn orders-for-restaurant
  [meals restaurant-name person]
  (let [restaurant-meals (filter #(= restaurant-name (-> (val %) :chosen-restaurant :name)) meals)]
    (map #(get-in (val %) [:people person :order]) restaurant-meals)))