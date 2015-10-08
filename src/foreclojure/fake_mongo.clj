(ns foreclojure.fake-mongo
  (:require [com.gfredericks.webscale :as webscale]))

(defn ^:private compile-where-map
  "Returns a predicate."
  [where-map]
  (apply every-pred
         (for [[k v] where-map]
           (if (re-find #"^[a-zA-Z]|_id$" (name k))
             (cond ((some-fn string? nil? #(instance? Boolean %)
                             integer?) v)
                   (fn [m] (= v (get m k)))

                   (and (map? v)
                        (= [:$in] (keys v)))
                   (let [xs (set (:$in v))]
                     (fn [m] (contains? xs (get m k))))

                   (and (map? v)
                        (= [:$nin] (keys v)))
                   (let [xs (set (:$in v))]
                     (fn [m] (not (contains? xs (get m k)))))

                   :elso
                   (throw (ex-info "Unknown value type!" {:where-map where-map})))
             (throw (ex-info "WTF" {:where-map where-map}))))))

(defn ^:private compile-actions
  [actions]
  (apply comp
         (for [[k v] actions]
           (case k
             :$set #(merge % v)
             :$addToSet (fn [rec]
                          (reduce (fn [rec [k item]]
                                    (update rec k (fnil conj #{}) item))
                                  rec
                                  v))

             (throw (Exception. "WTF??"))))))

(defmulti ^:private update-state
  (fn [state ev]
    (:type ev)))

(defn bad-next-uuid
  [seed]
  (let [r (java.util.Random. seed)
        [x1 x2 x3] (repeatedly #(.nextLong r))]
    [(java.util.UUID. x1 x2) x3]))

(defmethod update-state :insert
  [state {:keys [table doc]}]
  (let [{:keys [::seed]} state
        [id next-seed] (bad-next-uuid seed)
        doc (update doc :_id #(or % (str id)))]
    (-> state
        (assoc ::seed next-seed)
        (assoc-in [table (:_id doc)] doc))))

(defmethod update-state :update
  [state {:keys [table where actions opts] :as arg}]
  (let [pred (compile-where-map where)
        func (compile-actions actions)
        {:keys [upsert multiple]} opts
        {:keys [::seed]} state
        [random-id next-seed] (bad-next-uuid seed)

        new-state
        (update state table
                (fn [recs]
                  (let [[new-recs matched]
                        (reduce (fn [[new-recs matched] id]
                                  (let [rec (get new-recs id)]
                                    (if (pred rec)
                                      [(update new-recs id func) (inc matched)]
                                      [new-recs matched])))
                                [recs 0]
                                (keys recs))]
                    (when (and (not multiple) (< 1 matched))
                      (throw (ex-info "Multiple matches!"
                                      {:arg arg})))
                    (if (and (zero? matched)
                             upsert)
                      (let [new-id (java.util.UUID/randomUUID)]
                        (assoc new-recs new-id (func {:_id new-id})))
                      new-recs))))]
    (cond-> new-state
      (get-in new-state [table random-id])
      (assoc ::seed next-seed))))

(def ^:private the-db
  (webscale/create update-state {::seed 42} "fake-mongo"))

(defn ^:private records
  [table]
  (vals (get @the-db table)))

(defn insert!
  [table doc]
  (webscale/update! the-db
                    {:type :insert
                     :table table
                     :doc doc})
  :ok)

(defn update!
  [table where-map actions & {:as opts}]
  {:pre [(every? #{:$set :$addToSet} (keys actions))
         (every? #{:upsert :multiple} (keys opts))]}
  (webscale/update! the-db
                    {:type :update
                     :table table
                     :where where-map
                     :actions actions
                     :opts opts}))

(defmacro ^:private stubs
  [& names]
  (cons 'do
        (for [name names]
          `(defn ~name [& args#] (assert (not ~(format "IMPLEMENTED (%s)" name)))))))

(stubs fetch-and-modify destroy!)

(defmacro ^:private noops
  [& names]
  (cons 'do
        (for [name names]
          `(defn ~name [& args#]))))

(noops mongo! authenticate add-index!)

(defn fetch
  [table & {:keys [only where sort] :as opts}]
  {:pre [(every? #{:only :where :sort} (keys opts))]}
  (cond->> (records table)
    where
    (filter (compile-where-map where))

    only
    (map #(select-keys % only))

    sort
    (sort-by (case sort
               {:_id 1} :_id
               (throw (Exception. "WHAT SORT"))))))

(defn fetch-one
  [& args]
  (first (apply fetch args)))

(doseq [v (vals (ns-publics *ns*))
        :let [{:keys [name]} (meta v)]]
  (alter-var-root v
                  (fn [orig]
                    (fn [& args]
                      (try (apply orig args)
                           (catch Throwable t
                             (let [msg (str "Exception in " name)
                                   data {:args args}]
                               (print msg "")
                               (prn data)
                               (println (.getMessage t))
                               (throw (ex-info msg data t)))))))))
