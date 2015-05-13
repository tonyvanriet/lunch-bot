(ns lunch-bot.command.handler)


(defn dispatch-command->events [cmd] (:command-type cmd))

(defmulti command->events
          "converts the given command into events that should be committed to
          the event stream, if any."
          #'dispatch-command->events)


(defmethod command->events :default [_] nil)

(defmethod command->events :submit-payment
  [{:keys [amount to date] :as cmd}]
  [{:type   :paid
    :amount amount
    :to     to
    :date   date}])

(defmethod command->events :submit-bought
  [{:keys [amount date] :as cmd}]
  [{:type   :bought
    :amount amount
    :date   date}])

(defmethod command->events :submit-cost
  [{:keys [amount date] :as cmd}]
  [{:type   :cost
    :amount amount
    :date   date}])

(defmethod command->events :declare-in
  [{:keys [date] :as cmd}]
  [{:type :in
    :date date}])

(defmethod command->events :declare-out
  [{:keys [date] :as cmd}]
  [{:type :out
    :date date}])

(defmethod command->events :choose-restaurant
  [{:keys [restaurant date] :as cmd}]
  [{:type       :choose
    :restaurant restaurant
    :date       date}])

(defmethod command->events :submit-order
  [{:keys [food date] :as cmd}]
  [{:type :order
    :food food
    :date date}])

