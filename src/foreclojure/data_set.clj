(ns foreclojure.data-set
  (:use [foreclojure.fake-mongo]))

;;
;; Should the descriptions aim to actually introduce new concepts, or
;; just describe the problem and leave the cheatsheet for that?
;;
;; I'm leaning toward the latter
;;

(def the-problems
  [ ;;
   ;; "Simple" generators
   ;;

   {:title "Constancy is its own reward"
    :description "Create a generator that always generates 42."
    :tags ["simple"]
    :tests ["(gen/generator? __)"
            "(->> (gen/sample __ 1000) (every? #(= % 42)))"]
    :possible-answers '[(gen/return 42)]}
   {:title "Black and white"
    :description "Create a generator of true and false."
    :tags ["simple"]
    :tests ["(->> (gen/sample __ 1000) (every? #(instance? Boolean %)))"
            "(->> (gen/sample __ 1000) (frequencies) (vals) (every? #(< 400 %)))"]}
   {:title "Numbers!"
    :description "Create a generator of integers"
    :tags ["simple"]
    :tests ["(->> (gen/sample __ 1000) (every? integer?))"
            "(->> (gen/sample __ 1000) (distinct) (count) (< 50))"]}
   {:title "Strings"
    :description "Create a generator of strings of ASCII characters"
    :tags ["simple"]
    :tests ["(->> (gen/sample __ 1000) (every? string?))"
            "(->> (gen/sample __ 1000) (apply concat) (every? #(<= 0 % 127)))"
            "(->> (gen/sample __ 1000) (apply concat) (distinct) (count) (< 50))"
            ;; TODO: statsy tests
            ]}
   {:title "Enumerationalization"
    :description "Create a generator of :foo, :bar, or :baz."
    :tags ["simple"]
    :tests ["(->> (gen/sample __ 1000) (every? #{:foo :bar :baz}))"
            "(->> (gen/sample __ 1000) (distinct) (count) (= 3))"
            "(->> (gen/sample __ 1000) (frequencies) (vals) (every? #(< 200 %)))"]}

   ;;
   ;; Data structure generators
   ;;
   {:title "Lists of booleans"
    :description "Create a generator of lists of booleans."
    :tests ["(->> (gen/sample __ 1000) (every? seq?))"
            "(->> (gen/sample __ 1000) (apply concat) (every? #(instance? Boolean %)))"
            ;; TODO: statsy tests
            ]}
   {:title "Lists of five booleans"
    :description "Create a generator of lists of five booleans."
    :tests ["(->> (gen/sample __ 1000) (every? seq?))"
            "(->> (gen/sample __ 1000) (map count) (every? #(= 5 %)))"
            "(->> (gen/sample __ 1000) (apply concat) (every? #(instance? Boolean %)))"
            ;; TODO: statsy tests
            ]}
   {:title "Maps from strings to integers"
    :description "Create a generator of maps from strings to integers"
    :tests ["(->> (gen/sample __ 1000) (every? map?))"
            "(->> (gen/sample __ 1000) (mapcat keys) (every? string?))"
            "(->> (gen/sample __ 1000) (mapcat vals) (every? integer?))"
            ;; TODO: statsy tests
            ]}

   {:title "What a pear!"
    :description "Create a generator of pairs of booleans and integers, e.g. [true 42]."}

   {:description "Create a generator of maps with keys :name, :age, :height, and values of strings, integers, and doubles, respectively."}

   {:description "Create a generator of vectors of lists of pairs of maps from ints to ints and keywords, e.g. [([{2 3, 54 1} :heyo] [{} :what]) () ([{-1 1} :a-keyword])]"}

   ;;
   ;; Combinator generators
   ;;
   {:title "Even numbers!"
    :description "Create a generator that generates even numbers."
    :tests ["(->> (gen/sample __ 1000) (every? integer?))"
            "(->> (gen/sample __ 1000) (every? even?))"]}
   {:description "Create a generator of strings of integers, e.g. \"42\" or \"-17\"."}])

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
