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
  [;;
   ;; "Simple" generators
   ;;

   {:title "Constancy is its own reward"
    :description "Create a generator that always generates 42."
    :tags ["simple"]
    :tests '[(gen/generator? __)
             (->> (gen/sample __ 1000)
                  (every? #(= % 42)))]
    :good-answers '[(gen/return 42)]
    :bad-answers  '[gen/nat]}
   {:title "Black and white"
    :description "Create a generator of true and false."
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(instance? Boolean %)))
             (->> (gen/sample __ 1000)
                  (frequencies)
                  (vals)
                  (every? #(< 400 % 600)))]
    :good-answers '[gen/boolean
                    (gen/elements [true false])]
    :bad-answers '[gen/nat
                   (gen/return true)
                   (gen/return false)
                   (gen/fmap zero? gen/nat)]}
   {:title "Numbers!"
    :description "Create a generator of integers"
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (distinct)
                  (count)
                  (< 50))]
    :good-answers '[gen/nat
                    gen/int
                    gen/pos-int
                    gen/s-pos-int]
    :bad-answers '[(gen/return 42)
                   gen/boolean
                   gen/ratio]}
   {:title "Strings"
    :description "Create a generator of strings of ASCII characters"
    :tags ["simple"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (map int)
                  (every? #(<= 0 % 127)))
             (->> (gen/sample __ 1000)
                  ;; check variety of characters
                  (apply concat)
                  (distinct)
                  (count)
                  (< 50))
             ;; TODO: statsy tests
             ]
    :good-answers '[gen/string-ascii]
    :bad-answers '[gen/string
                   gen/nat
                   gen/keyword
                   (gen/vector gen/string-ascii)
                   (gen/return "blah")
                   (gen/fmap str gen/nat)]}
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
                  (every? #(< 200 %)))]
    :good-answers '[(gen/elements [:foo :bar :baz])]
    :bad-answers '[(gen/elements [:foo :bar])
                   (gen/elements [:foo :bar :baz :booze])
                   (gen/return :foo)
                   gen/keyword]}

   ;;
   ;; Data structure generators
   ;;
   {:title "A list is a variable-length homogeneous collection"
    :description "Create a generator of lists of booleans."
    :tags ["collections"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? seq?))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (every? #(instance? Boolean %)))
             (let [[c1 c2]
                   (->> (gen/sample __ 1000)
                        (apply concat)
                        (frequencies)
                        (vals))]
               (< 1/2 (/ c1 c2) 3/2))
             ;; TODO: statsy tests
             ]
    :good-answers '[(gen/list gen/boolean)]
    :bad-answers '[gen/boolean
                   (gen/list (gen/return true))
                   (gen/list gen/nat)
                   (gen/return '(true false true))]}
   {:title "A list is a fixed-length homogeneous collection"
    :description "Create a generator of lists of five booleans."
    :tags ["collections"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? sequential?))
             (->> (gen/sample __ 1000)
                  (map count)
                  (every? #(= 5 %)))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (every? #(instance? Boolean %)))
             ;; this happens before 333 almost all of the time
             (->> (gen/sample __ 1000)
                  ;; should generate all possible values
                  (distinct)
                  (count)
                  (= 32))
             ;; TODO: statsy tests
             ]
    :good-answers '[(apply gen/tuple
                           (repeat 5 gen/boolean))
                    (gen/vector gen/boolean 5)]
    :bad-answers '[(gen/list gen/boolean)
                   (apply gen/tuple
                          (gen/repeat 5 (gen/return false)))
                   (gen/return (repeat 5 true))]}

   {:title "A list is a fixed-length heterogeneous collection"
    :description "Create a generator of pairs of booleans and integers, e.g. <code>[true 42]</code>."
    :tags ["collections"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(and (vector? %)
                                (= 2 (count %)))))
             (->> (gen/sample __ 1000)
                  (map first)
                  (every? #(instance? Boolean %)))
             (->> (gen/sample __ 1000)
                  (map second)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  ;; generates a lot of different values
                  (distinct)
                  (count)
                  (< 200))
             ;; TODO: statsy tests
             ]
    :good-answers '[(gen/tuple gen/boolean gen/int)]
    :bad-answers '[(gen/tuple gen/boolean)
                   (gen/vector (gen/one-of gen/boolean gen/int) 2)
                   (gen/tuple gen/boolean gen/int gen/int)
                   (gen/return [true 42])]}

   {:title "A map is like a mathematical function."
    :description "Create a generator of maps from strings to integers"
    :tags ["collections"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? map?))
             (->> (gen/sample __ 1000)
                  (mapcat keys)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (mapcat vals)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (some empty?))
             (->> (gen/sample __ 1000)
                  ;; generates lots of different keys
                  (mapcat keys)
                  (distinct)
                  (count)
                  (< 1000))
             (->> (gen/sample __ 1000)
                  ;; generates lots of different value sets
                  (map vals)
                  (distinct)
                  (count)
                  (< 500))
             (->> (gen/sample __ 1000)
                  ;; Has a variety of sizes
                  (map count)
                  (distinct)
                  (count)
                  (< 20))
             ;; TODO: statsy tests
             ]
    :good-answers '[(gen/map gen/string-ascii gen/int)
                    (gen/fmap (fn [[ks vs]] (zipmap ks vs))
                              (gen/tuple (gen/list gen/string)
                                         (gen/list gen/int)))]
    :bad-answers '[(gen/return {})
                   (gen/return {"hey" 42})
                   (gen/fmap (fn [[k v]] {k v})
                             (gen/tuple gen/string))]}

   {:title "A map is like a row in a database."
    :description "Create a generator of maps with keys :name, :age, :height, and values of strings, integers, and doubles, respectively."
    :tags ["collections"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(and (map? %)
                                (= #{:name :age :height}
                                   (set (keys %))))))
             (->> (gen/sample __ 1000)
                  (map (juxt :name :age :height))
                  (map #(map type %))
                  (every? #(= % [String Long Double])))
             (->> (gen/sample __ 1000)
                  ;; Generates a variety of values
                  (distinct)
                  (count)
                  (< 900))
             ;; TODO: statsy tests
             ]
    :good-answers '[(gen/hash-map :name gen/string
                                  :age gen/nat
                                  ;; TODO: make a real double generator!
                                  :height (gen/fmap double gen/ratio))]
    :bad-answers '[(gen/return {:name "Gary" :age 45 :height 20.0})
                   (gen/hash-map :name gen/string
                                 :age gen/nat
                                 :height (gen/fmap double gen/ratio)
                                 :opinions #{})]}

   {:title "Do I really have to do this one?"
    :description "Create a generator of vectors of lists of pairs of maps from ints to ints and keywords, e.g. [([{2 3, 54 1} :heyo] [{} :what]) () ([{-1 1} :a-keyword])]"
    :tags ["collections" "bonus"]
    :tests '[(->> (gen/sample (gen/scale #(min % 15) __) 100)
                  (every? vector?))
             (->> (gen/sample (gen/scale #(min % 15) __) 100)
                  (apply concat)
                  (every? seq?))
             (->> (gen/sample (gen/scale #(min % 15) __) 100)
                  (apply concat)
                  (apply concat)
                  (map first)
                  (every? map?))
             (->> (gen/sample (gen/scale #(min % 15) __) 100)
                  ;; generates a variety of maps & keywords
                  (tree-seq sequential? identity)
                  (distinct)
                  (count)
                  (< 100))]
    :good-answers '[(gen/vector (gen/list (gen/tuple (gen/map gen/int gen/int)
                                                     gen/keyword)))]
    :bad-answers '[(gen/return [([{2 3, 54 1} :heyo] [{} :what])
                                ()
                                ([{-1 1} :a-keyword])])]}

   ;;
   ;; Combinator generators
   ;;
   {:title "Even numbers!"
    :description "Create a generator that generates even numbers."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  (every? even?))
             (->> (gen/sample __ 1000)
                  (distinct)
                  (count)
                  (< 50))]
    :good-answers '[(gen/fmap #(* % 2) gen/int)]
    :bad-answers '[(gen/return 42)
                   (gen/such-that even? gen/int)]}
   {:title "Three-valued logic"
    :description "Create a generator that generates booleans and nil, roughly equally often."
    :tests '[(->> (gen/sample __ 1000)
                  (every? #(contains? #{true false nil} %)))
             (->> (gen/sample __ 1000)
                  ;; Decent distribution
                  (frequencies)
                  (vals)
                  (every? #(< 250 % 400)))]
    :good-answers '[(gen/elements [true false nil])]
    :bad-answers '[(gen/return true)
                   (gen/return nil)
                   (gen/elements [true false nil 42])]}
   {:title "[INSERT CLEVER PROBLEM TITLE]"
    :description "Create a generator that sometimes generates keywords and sometimes generates pairs of booleans."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? (fn [x] (or (keyword? x)
                                      (and (vector? x)
                                           (= 2 (count x))
                                           (every? #(instance? Boolean %) x))))))
             (->> (gen/sample __ 1000)
                  ;; Generates each type roughly equally often
                  (map type)
                  (frequencies)
                  (vals)
                  (every? #(< 300 % 700)))
             (->> (gen/sample __ 1000)
                  ;; Generates a good variety of values
                  (distinct)
                  (count)
                  (< 400))
             (->> (gen/sample __ 1000)
                  ;; Generates all possible boolean pairs
                  (filter vector?)
                  (distinct)
                  (count)
                  (= 4))]
    :good-answers '[(gen/one-of [gen/keyword (gen/tuple gen/boolean gen/boolean)])]
    :bad-answers '[(gen/return :foo)
                   (gen/return [true false])
                   (gen/elements [:foo :bar :baz [true false] [true true] [false false]])]}
   {:title "Stringly typed"
    :description "Create a generator of strings of integers, e.g. <code>\"42\"</code> or <code>\"-17\"</code>."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? string?))
             (->> (gen/sample __ 1000)
                  (map #(Long/parseLong %))
                  (every? integer?))
             (->> (gen/sample __ 1000)
                  ;; Generates an okay variety
                  (distinct)
                  (count)
                  (< 50))]
    :good-answers '[(gen/fmap str gen/int)]
    :bad-answers '[(gen/return "42")]}

   {:title "Pick a card, any card"
    :description "Create a generator of [xs x], where xs is a non-empty list of integers and x is an element of xs."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  ;; Correct shape
                  (every? (fn [[xs x]]
                            (and (seq? xs)
                                 (every? integer? xs)
                                 (integer? x)))))
             (->> (gen/sample __ 1000)
                  ;; The core requirements
                  (every? (fn [[xs x]]
                            (some #(= x %) xs))))
             (->> (gen/sample __ 1000)
                  ;; Should generate a good variety of things
                  (distinct)
                  (count)
                  (< 600))]
    :good-answers '[(gen/bind (gen/not-empty (gen/list gen/int))
                              (fn [xs]
                                (gen/tuple (gen/return xs)
                                           (gen/elements xs))))]
    :bad-answers '[(gen/return [[42] 42])
                   (gen/tuple (gen/list gen/nat) gen/nat)
                   (gen/bind (gen/list gen/int)
                             (fn [xs]
                               (gen/tuple (gen/return xs)
                                          (gen/elements xs))))
                   (gen/such-that
                    (fn [[xs x]] (some #{x} xs))
                    (gen/tuple (gen/list gen/nat) gen/nat))]}

   {:title "It's sort of like the matrix"
    :description "Create a generator of vectors of vectors of integers, where the inner vectors are all the same size."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? (fn [v]
                            (and (vector? v)
                                 (every? vector? v)
                                 (every? integer? (apply concat v))))))
             (->> (gen/sample __ 1000)
                  (every? (fn [v]
                            (or (empty? v)
                                (apply = (map count v))))))
             (->> (gen/sample __ 1000)
                  ;; Generates a good variety of values
                  (distinct)
                  (count)
                  (< 900))
             (->> (gen/sample __ 1000)
                  ;; Generates a good variety of inner sizes
                  (apply concat)
                  (map count)
                  (distinct)
                  (count)
                  (< 10))
             (->> (gen/sample __ 1000)
                  ;; Generates a good variety of numbers
                  (apply concat)
                  (apply concat)
                  (distinct)
                  (count)
                  (< 50))]
    :good-answers '[(gen/bind gen/nat
                              (fn [len]
                                (gen/vector (gen/vector gen/int len))))]
    :bad-answers '[(gen/return [])
                   (gen/return [[]])
                   (gen/return [[1 2 3]])
                   (gen/return [[1 2 3] [4 5 6] [7 8 9]])
                   (gen/vector (gen/vector gen/int))]}

   ;; TODO: will these tests allow a top-level such-that? (I hope not)
   {:title "Sets can't contain 42."
    :description "Create a generator of sets of integers that never contain 42."
    :tags ["combinators"]
    :tests '[(->> (gen/sample __ 1000)
                  (every? (fn [xs]
                            (and (set? xs)
                                 (every? integer? xs)))))
             (->> (gen/sample __ 1000)
                  (apply concat)
                  (not-any? #(= % 42)))
             (->> (gen/sample __ 1000)
                  ;; Generates a good variety of values
                  (distinct)
                  (count)
                  (< 800))]
    ;; TODO: proper set generator
    :good-answers '[(gen/fmap set (gen/list (gen/such-that #(not= % 42) gen/int)))]
    :bad-answers '[(gen/return #{1 2 3 4})
                   (gen/such-that #(not (% 42))
                                  (gen/set gen/int))]}])

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
            :let [prob (reduce (fn [prob k]
                                 (update prob k
                                         (fn [forms]
                                           (map (fn [form]
                                                  (if (symbol? form)
                                                    (pr-str form)
                                                    (read-source this-file
                                                                 (meta form))))
                                                forms))))
                               prob
                               [:tests :good-answers :bad-answers])]]
      (insert! :problems
               (assoc prob
                      :_id id
                      :times-solved 0
                      :approved true)))))

(defonce check-solution
  ;; "Returns nil for success, or an error message"
  (memoize
   (fn [problem-id tests code-str]
     (:error (foreclojure.problems/run-code* problem-id code-str)))))

(defn test-good-answers
  []

  (doseq [{:keys [_id bad-answers good-answers tests title]}
          (sort-by :_id
                   (#'foreclojure.fake-mongo/records :problems))]
    (doseq [code-str good-answers]
      (when-let [error (check-solution _id tests code-str)]
        (println "CRAP" _id code-str error)))
    (doseq [code-str bad-answers]
      (when-not (check-solution _id tests code-str)
        (println "CRAP[ASSED]" _id code-str))))
  (println "Done."))

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
