(ns lunch-bot.money
  (:gen-class))


(defn balance-change
  [person amount]
  {:person person :amount amount})


(defn event->balance-changes
  [{:keys [type person recipient amount]}]
  (cond (= type :bought) (balance-change person amount)
        (= type :cost)   (balance-change person (* -1 amount))
        (= type :paid)   [(balance-change person amount)
                          (balance-change recipient (* -1 amount))]))


(defn events->balance-changes
  [events]
  (->> events
       (map #(event->balance-changes %))
       (flatten)))


(defn events->balances
  "turns a stream of events into a map of balances for each person"
  [events]
  (let [balance-changes (events->balance-changes events)
        balance-changes-by-person (group-by #(:person %) balance-changes)]
    (into {} (for [[person balance-changes] balance-changes-by-person]
               [person (reduce + (map #(:amount %) balance-changes))]))))


(defn apply-event
  [balances event]
  (let [event-balances (events->balances [event])]
    (merge-with + balances event-balances)))


(defn apply-events
  [balances events]
  (reduce #(apply-event %1 %2) balances events))


(defn sort-balances
  [balances]
  (sort-by val balances))


(defn biggest-payoff
  "determines the largest payoff that can be made between two people to bring
  one of their balances to zero. returns nil if there are no negative balances
  to payoff."
  [balances]
  (let [sorted-balances (sort-balances balances)
        min-balance (first sorted-balances)
        max-balance (last sorted-balances)
        amount (min (* -1 (val min-balance)) (val max-balance))]
    {:person (key min-balance)
     :type :paid
     :amount amount
     :recipient (key max-balance)}))


(defn minimal-payoffs
  "returns a set of payments that could be made to eliminate all debt in the
  shortest number of payments"
  [balances]
  (loop [payments []
         balances balances]
    (if (not-any? #(< (val %) 0) balances)
      payments
      (let [payment (biggest-payoff balances)]
        (recur (conj payments payment)
               (apply-event balances payment))))))


(defn consolidation-payoffs
  "returns a set of payments that could be made to consolidate all of the debtor's
  debt to the consolidatee."
  [balances debtor consolidatee])

