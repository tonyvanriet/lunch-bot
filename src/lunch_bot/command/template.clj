(ns lunch-bot.command.template
  (:require [clj-time.core :as time]))


(defn command-template->element-keys
  [template]
  (map #(first %) template))


(defmulti command-template->command #'command-template->element-keys)

(defmethod command-template->command :default [_]
  nil)

(defmethod command-template->command [:help]
  [[[_ _]]]
  {:command-type :help})

;
; show templates
;
(defmethod command-template->command [:show :noun]
  [[[_ _] [_ noun]]]
  {:command-type :show
   :info-type    noun})

(defmethod command-template->command [:noun]
  [[noun-elem]]
  (command-template->command [[:show :show] noun-elem]))

(defmethod command-template->command [:show :date]
  [[[_ _] [_ date]]]
  {:command-type :show
   :info-type    :meal-summary
   :date         date})

(defmethod command-template->command [:date]
  [[date-elem]]
  (command-template->command [[:show :show] date-elem]))

;
; paid templates
;
(defmethod command-template->command [:paid :user :amount]
  [[[_ _] [_ user-id] [_ amount]]]
  {:command-type :submit-payment
   :amount       amount
   :to           user-id
   :date         (time/today)})

(defmethod command-template->command [:paid :amount :user]
  [[action-elem amount-elem user-elem]]
  (command-template->command [action-elem user-elem amount-elem]))

;
; borrowed/owe templates
;
(defmethod command-template->command [:borrowed :user :amount]
  [[[_ _] [_ user-id] [_ amount]]]
  {:command-type :submit-debt
   :amount       amount
   :from         user-id
   :date         (time/today)})

(defmethod command-template->command [:borrowed :amount :user]
  [[action-elem amount-elem user-elem]]
  (command-template->command [action-elem user-elem amount-elem]))

;
; bought templates
;
(defmethod command-template->command [:bought :date :amount]
  [[[_ _] [_ date] [_ amount]]]
  {:command-type :submit-bought
   :amount       amount
   :date         date})

(defmethod command-template->command [:bought :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->command [action-elem date-elem amount-elem]))

(defmethod command-template->command [:bought :amount]
  [[action-elem amount-elem]]
  (command-template->command [action-elem [:date (time/today)] amount-elem]))

;
; cost templates
;
(defmethod command-template->command [:cost :date :amount :+tax]
  [[[_ _] [_ date] [_ amount] [_ +tax]]]
  {:command-type :submit-cost
   :amount       amount
   :+tax?        +tax
   :date         date})

(defn get-default-cost-date-elem [] [:date (time/today)])
(defn get-default-cost-+tax-elem [] [:+tax nil])

(defmethod command-template->command [:cost :date :amount]
  [[action-elem date-elem amount-elem]]
  (command-template->command [action-elem date-elem amount-elem (get-default-cost-+tax-elem)]))

(defmethod command-template->command [:cost :amount :date]
  [[action-elem amount-elem date-elem]]
  (command-template->command [action-elem date-elem amount-elem (get-default-cost-+tax-elem)]))

(defmethod command-template->command [:cost :amount :+tax]
  [[action-elem amount-elem +tax-elem]]
  (command-template->command [action-elem (get-default-cost-date-elem) amount-elem +tax-elem]))

(defmethod command-template->command [:cost :amount :+tax :date]
  [[action-elem amount-elem +tax-elem date-elem]]
  (command-template->command [action-elem date-elem amount-elem +tax-elem]))

(defmethod command-template->command [:cost :amount]
  [[action-elem amount-elem]]
  (command-template->command [action-elem (get-default-cost-date-elem) amount-elem (get-default-cost-+tax-elem)]))


(defmethod command-template->command [:diners :amount]
  [[[_ _] [_ amount]]]
  {:command-type :declare-diners
   :number       (int amount)
   :date         (time/today)})

(defmethod command-template->command [:in]
  [[[_ _]]]
  {:command-type :declare-in
   :date         (time/today)})

(defmethod command-template->command [:out]
  [[[_ _]]]
  {:command-type :declare-out
   :date         (time/today)})

(defmethod command-template->command [:suggest :restaurant]
  [[[_ _] [_ restaurant]]]
  {:command-type :suggest-restaurant
   :restaurant   restaurant
   :date         (time/today)})

(defmethod command-template->command [:choose :restaurant]
  [[[_ _] [_ restaurant]]]
  {:command-type :choose-restaurant
   :restaurant   restaurant
   :date         (time/today)})

(defmethod command-template->command [:order :food]
  [[[_ _] [_ food]]]
  {:command-type :submit-order
   :food         food
   :date         (time/today)})

(defmethod command-template->command [:nag :date]
  [[[_ _] [_ date]]]
  {:command-type :find-nags
   :date         date})

(defmethod command-template->command [:nag]
  [[action-elem]]
  {:command-type :find-nags
   :date         (time/today)})
