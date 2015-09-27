(ns foreclojure.fake-mongo)

(def the-db)

(defn insert!
  [table doc]
  (assert (:_id doc))
  (swap! the-db assoc-in [table (:_id doc)] doc)
  :ok)

(defn all-records
  [table]
  (vals (doto (get @the-db table) assert)))

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
