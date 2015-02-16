(ns lunch-bot.meal)


;;
;; a meal contains the set of people that are in for lunch, the set that
;; are out, and a collection of restaurants that have been chosen along
;; with order info for each restaurant.
;;


(def empty-meal
  {:ins    #{}
   :outs   #{}
   :chosen []})


(def example-meal
  {:ins    #{"bob" "rozz"}
   :outs   #{"steve"}
   :chosen [{:restaurant {:name "BW3"}
             :ins        [{:person "bob"
                           :order  "jumbo dog\nno hot peppers\nlg fry"
                           :cost   8.5M}
                          {:person "rozz"
                           :order  "bowl of chili"
                           :cost   9M}
                          {:person "biff"}]
             :outs       [{:person "steve"}
                          {:person "judy"}]
             :bought     {:person "bob"
                          :amount 45.75M}}
            {:restaurant {:name "Portillos"}
             :ins        [{:person "loner"}]
             :outs       []}]})


(defmulti apply-event-to-meal #(:type %2))

(defmethod apply-event-to-meal :in
  [{:keys [ins outs] :as meal}
   {:keys [person] :as event}]
  (assoc meal :ins (conj ins person)
              :outs (disj outs person)))

(defmethod apply-event-to-meal :out
  [{:keys [ins outs] :as meal}
   {:keys [person] :as event}]
  (assoc meal :outs (conj outs person)
              :ins (disj ins person)))

(defmethod apply-event-to-meal :default
  [meal _]
  meal)


(defn events->meal
  "constructs an aggregate view of the meal for a given day from the
  list of meal-events."
  [events]
  (reduce apply-event-to-meal empty-meal events))

