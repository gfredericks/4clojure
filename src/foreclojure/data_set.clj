(ns foreclojure.data-set
  (:use [foreclojure.fake-mongo]))

(def the-problems
  [{:title "Constancy is its own reward"
    :description "`(gen/return x)` is a generator that always generates `x`"
    :tags ["elementary"]
    :tests ["(gen/generator? __)"
            "(every? #(= % 42) (gen/sample __ 1000))"]}])

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
