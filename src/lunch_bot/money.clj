(ns lunch-bot.money
  (:gen-class))


(defn balance-change
  [person amount]
  {:person person :amount amount})


(defn event->balance-changes
  [event]
  (cond (= (:type event) :bought) (balance-change (:person event) (:amount event))
        (= (:type event) :cost) (balance-change (:person event) (* -1 (:amount event)))
        (= (:type event) :paid) [(balance-change (:person event) (:amount event))
                                 (balance-change (:recipient event) (* -1 (:amount event)))]))


(defn events->balance-changes
  [events]
  (->> events
       (map #(event->balance-changes %))
       (flatten)))


(defn balances->balance
  [balances person]
  (first (filter #(= :person person) balances)))


(defn update-balances
  [balances balance-change]
  (let [person (:person balance-change)
        pre-balance (get balances person 0)]
    (assoc balances person (+ pre-balance (:amount balance-change)))))


(defn events->balances
  "turns a stream of events into a map of balances for each person"
  [events]
  (let [balance-changes (events->balance-changes events)]
    (reduce update-balances {} balance-changes)))



(defn minimize-debt
  [events]
  nil)

