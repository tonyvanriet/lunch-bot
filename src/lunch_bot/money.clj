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


(defn minimize-debt
  [events]
  nil)



