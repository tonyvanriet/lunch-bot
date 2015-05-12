(ns lunch-bot.command.handler)


(defn dispatch-command->events [cmd agg] (:command-type cmd))

(defmulti command->events
          "converts the given command into events that should be committed to
          the events stream, if any."
          #'dispatch-command->events)


(defmethod command->events :add-payment
  [{:keys [amount to date] :as cmd} committed-events]
  [{:type   :paid
    :amount amount
    :to     to
    :date   date}])

