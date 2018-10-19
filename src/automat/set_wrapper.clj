(ns automat.set-wrapper
  (:require [clojure.set :as set]))

;;make sure that set functions return set

(defn union [& xs]
  (->> (map #(into #{} %) xs)
       (apply set/union)))

(defn intersection [x & xs]
  (->> (map #(into #{} %) xs)
       (apply set/intersection (into #{} x))))

(defn difference [x & xs]
  (->> (map #(into #{} %) xs)
       (apply set/difference (into #{} x))))
