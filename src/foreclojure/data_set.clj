(ns foreclojure.data-set
  (:use [foreclojure.fake-mongo]))

(def this-file *file*)

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
    :tests '[(gen/generator? __)
             (->> (gen/sample __ 1000)
                  (every? #(= % 42)))]
    :possible-answers '[(gen/return 42)]}
   {:title "Black and white"
    :description "Create a generator of true and false."
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(instance? Boolean %)))
             (->> (gen/sample __ 1000)
                  (frequencies)
                  (vals)
                  (every? #(< 400 %)))]}
   {:title "Numbers!"
    :description "Create a generator of integers"
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (distinct)
                  (count)
                  (< 50))]}
   {:title "Strings"
    :description "Create a generator of strings of ASCII characters"
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? string?))
             (->> (gen/sample __ 1000) (apply concat)
                  (every? #(<= 0 % 127)))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (distinct)
                  (count)
                  (< 50))
             ;; TODO: statsy tests
             ]}
   {:title "Enumerationalization"
    :description "Create a generator of :foo, :bar, or :baz."
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? #{:foo :bar :baz}))
             (->> (gen/sample __ 1000)
                  (distinct)
                  (count)
                  (= 3))
             (->> (gen/sample __ 1000)
                  ;; check that value appears at least 200 times
                  (frequencies)
                  (vals)
                  (every? #(< 200 %)))]}

   ;;
   ;; Data structure generators
   ;;
   {:title "A list is a variable-length homogeneous collection"
    :description "Create a generator of lists of booleans."
    :tests '[(->> (gen/sample __ 1000)
                  (every? seq?))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (every? #(instance? Boolean %)))
             ;; TODO: statsy tests
             ]}
   {:title "A list is a fixed-length homogeneous collection"
    :description "Create a generator of lists of five booleans."
    :tests '[(->> (gen/sample __ 1000)
                  (every? seq?))
             (->> (gen/sample __ 1000)
                  (map count)
                  (every? #(= 5 %)))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (every? #(instance? Boolean %)))
             ;; TODO: statsy tests
             ]}

   {:title "A list is a fixed-length heterogeneous collection"
    :description "Create a generator of pairs of booleans and integers, e.g. <code>[true 42]</code>."
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(and (vector? %)
                                (= 2 (count %)))))
             (->> (gen/sample __ 1000)
                  (map first)
                  (every? #(instance? Boolean %)))
             (->> (gen/sample __ 1000)
                  (map second)
                  (every? integer?))
             ;; TODO: statsy tests
             ]}

   {:title "A map is like a mathematical function."
    :description "Create a generator of maps from strings to integers"
    :tests '[(->> (gen/sample __ 1000)
                  (every? map?))
             (->> (gen/sample __ 1000)
                  (mapcat keys)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (mapcat vals)
                  (every? integer?))
             ;; TODO: statsy tests
             ]}

   {:title "A map is like a row in a database."
    :description "Create a generator of maps with keys :name, :age, :height, and values of strings, integers, and doubles, respectively."
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(and (map? %)
                                (= #{:name :age :height}
                                   (set (keys %))))))
             (->> (gen/sample __ 1000)
                  (map :name)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (map :age)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (map :height)
                  (every? float?))
             ;; TODO: statsy tests
             ]
    }

   {:title "Do I really have to do this one?"
    :description "Create a generator of vectors of lists of pairs of maps from ints to ints and keywords, e.g. [([{2 3, 54 1} :heyo] [{} :what]) () ([{-1 1} :a-keyword])]"
    :tests '[(->> (gen/sample __ 1000)
                  (every? vector?))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (every? seq?))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (apply concat)
                  (map first)
                  (every? map?))]}

   ;;
   ;; Combinator generators
   ;;
   {:title "Even numbers!"
    :description "Create a generator that generates even numbers."
    :tests '[(->> (gen/sample __ 1000)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (every? even?))]}
   {:title "[INSERT CLEVER PROBLEM TITLE]"
    :description "Create a generator that sometimes generates keywords and sometimes generates pairs of booleans."
    :tests '[(->> (gen/sample __ 1000)
                  (every? (fn [x] (or (keyword? x)
                                      (and (vector? x)
                                           (= 2 (count x))
                                           (every? #(instance? Boolean %) x))))))]}
   {:title "Stringly typed"
    :description "Create a generator of strings of integers, e.g. \"42\" or \"-17\"."
    :tests '[(->> (gen/sample __ 1000)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (map #(Long/parseLong %))
                  (every? integer?))]}

   {:title "It's sort of like the matrix"
    :description "Create a generator of vectors of vectors of integers, where the inner vectors are all the same size."
    :tests '[(->> (gen/sample __ 1000)
                  (every? (fn [v]
                            (and (vector? v)
                                 (every? vector? v)
                                 (every? integer? (apply concat v))))))
             (->> (gen/sample __ 1000)
                  (every? (fn [v]
                            (or (empty? v)
                                (apply = (map count v))))))]}])

(defn read-source
  [filepath {:keys [line column]}]
  (with-open [is (-> filepath (clojure.java.io/input-stream))
              r (java.io.InputStreamReader. is)
              rdr (java.io.LineNumberReader. r)]
    (dotimes [_ (dec line)] (.readLine rdr))
    (dotimes [_ (dec column)] (.read rdr))
    (let [text (StringBuilder.)
          pbr (proxy [java.io.PushbackReader] [rdr]
                (read [] (let [i (proxy-super read)]
                           (.append text (char i))
                           i)))]
      (read {} (java.io.PushbackReader. pbr))
      (let [lines (clojure.string/split (str text) #"\n")]
        (->> (rest lines)
             (map #(subs % (dec column)))
             (cons (first lines))
             (clojure.string/join "\n"))))))

(defn load-problems []
  (do
    (insert! :seqs
             {:_id "problems"
              :seq (count the-problems)})
    (doseq [[prob id] (map vector the-problems (rest (range)))
            :let [prob (update prob :tests
                               (fn [tests]
                                 (map (fn [test]
                                        (read-source this-file
                                                     (meta test)))
                                      tests)))]]
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

  ;;
  ;; Dev utils
  ;;

  (defn reset-db
    []
    (.delete (java.io.File. "fake-mongo/data-0.edn"))
    (alter-var-root #'foreclojure.fake-mongo/the-db
                    (constantly (com.gfredericks.webscale/create
                                 #'foreclojure.fake-mongo/update-state
                                 {} "fake-mongo")))
    (foreclojure.mongo/prepare-mongo))

  )
