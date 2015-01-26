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


(defn sort-balances
  [balances]
  (sort-by val balances))


(defn biggest-payment
  [balances]
  (let [sorted-balances (sort-balances balances)
        min-balance (first sorted-balances)
        max-balance (last sorted-balances)
        amount (min (* -1 (val min-balance)) (val max-balance))]
    {:person (key min-balance)
     :type :paid
     :amount amount
     :recipient (key max-balance)}))


(defn minimize-debt
  [balances]
  (let [payment (biggest-payment balances)]
    [payment (apply-event balances payment)]))

