(ns foreclojure.data-set
  (:use [foreclojure.fake-mongo]))

;;
;; Should the descriptions aim to actually introduce new concepts, or
;; just describe the problem and leave the cheatsheet for that?
;;
;; I'm leaning toward the latter
;;

(def the-problems
  [{:title "Constancy is its own reward"
    :description "`(gen/return x)` is a generator that always generates `x`"
    :tags ["elementary"]
    :tests ["(gen/generator? __)"
            "(->> (gen/sample __ 1000) (every? #(= % 42)))"]
    :possible-answers '[(gen/return 42)]}
   {:title "Numbers!"
    :description "Generate some numbers"
    :tags ["elementary"]
    :tests ["(->> (gen/sample __ 1000) (every? integer?))"]}])

(defn load-problems []
  (do
    (insert! :seqs
             {:_id "problems"
              :seq (count the-problems)})
    (doseq [[prob id] (map vector the-problems (rest (range)))]
      (insert! :problems
               (assoc prob
                      :_id id
                      :times-solved 0
                      :approved true)))))


(comment

  ;; working out tests

  (require '[clojure.test.check.generators :as gen])

  (defn avg [x y] (/ (+ x y) 2))

  (defn recommend-bounds
    [gen pred sample-count]
    (let [[the-min the-max]
          (->> (repeatedly 1000 (fn []
                                  (->> (gen/sample gen sample-count)
                                       (filter pred)
                                       (count))))
               (map #(/ % sample-count))
               (apply (juxt min max)))]
      [(avg 0 the-min) (avg the-max 1)]))


  )
