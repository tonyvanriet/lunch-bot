(ns lunch-bot.restaurant
  (:require [lunch-bot.store :as store]))


(def restaurants-filename "restaurants.edn")


(def ^:private restaurants (atom []))


(defn initialize-restaurants []
  (reset! restaurants (vec (store/read-restaurants restaurants-filename))))


(defn get-restaurants
  []
  @restaurants)

