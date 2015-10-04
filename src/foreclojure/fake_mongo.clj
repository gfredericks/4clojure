(ns foreclojure.fake-mongo
  (:require [com.gfredericks.webscale :as webscale]))

(defmulti update-state
  (fn [state ev]
    (:type ev)))

(defmethod update-state :insert
  [state {:keys [table doc]}]
  (assert (:_id doc))
  (assoc-in state [table (:_id doc)] doc))

(def the-db
  (webscale/create update-state {} "fake-mongo"))

(defn insert!
  [table doc]
  (webscale/update! the-db
                    {:type :insert
                     :table table
                     :doc doc})
  :ok)

(defn all-records
  [table]
  (vals (get @the-db table {})))

(defn get-by-id
  [table id]
  (get-in @the-db [table id]))

(defn get-by
  [table & kvs]
  (let [pred (->> kvs
                  (partition 2)
                  (map (fn [[k v]]
                         #(= (k %) v)))
                  (apply every-pred))]
    (->> (get @the-db table)
         (vals)
         (filter pred))))

(defn get-one-by
  [table & kvs]
  (first (apply get-by table kvs)))
