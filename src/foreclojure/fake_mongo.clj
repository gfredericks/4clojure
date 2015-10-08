(ns foreclojure.fake-mongo
  (:require [com.gfredericks.webscale :as webscale]))

(defn ^:private bad-get-uuids
  "Don't use this haha."
  [seed]
  (let [r (java.util.Random. seed)]
    (->> (repeatedly #(.nextLong r))
         (partition 2)
         (map (fn [[x1 x2]]
                (java.util.UUID. x1 x2))))))

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

(defmulti ^:private update-state*
  (fn [state ev]
    (:type ev)))


(defmethod update-state* :insert
  [state {:keys [table doc]}]
  (let [id (-> state ::ev-num bad-get-uuids first)
        doc (update doc :_id #(or % (str id)))]
    (assoc-in state [table (:_id doc)] doc)))

(defmethod update-state* :update
  [state {:keys [table where actions opts] :as arg}]
  (let [pred (compile-where-map where)
        func (compile-actions actions)
        {:keys [upsert multiple]} opts
        random-uuids (bad-get-uuids (::ev-num state))]
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
                  (let [new-id (first random-uuids)]
                    (assoc new-recs new-id (func {:_id new-id})))
                  new-recs))))))

(defn update-state
  [state ev]
  (-> state
      (update-state* ev)
      (update ::ev-num inc)))

(def ^:private the-db
  (webscale/create update-state {::ev-num 0} "fake-mongo"))

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
