(ns lunch-bot.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [lunch-bot.aggregate :as aggr]))

(defroutes app
           (GET "/balances" [] (str (aggr/balances)))
           (GET "/all" [] (str (aggr/get-aggregates))))