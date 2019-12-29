(ns structural.core-test
  (:require [clojure.test :refer :all]
            [structural.core :refer :all]))

(comment ;testing

  (defrecord botmove [path position]) 
  (binding [*print-meta* true]
    (pprint
     (macroexpand-all
      '(with-slots [{:fields [^clojure.lang.Counted path  ^clojure.lang.Indexed position]} ^botmove move
                    [x y]         pos
                    path-length   (.count path)
                    blah  2
                    blee  (map inc (range 10))]
         2))))

  (with-slots [{:fields [^clojure.lang.Counted path
                         ^clojure.lang.Indexed position]} ^botmove (->botmove [] [1 2])
               {:keys [a b] :fields [hashCode]}    {:a 2 :b 3}
                [x y]          position
               path-length   (.count path)]
    [hashCode (+ x y)])

#_(binding [*print-meta* true]
  (pprint
   (macroexpand-1
    (macroexpand-1
     '(with-slots  [{:keys [a] :fields [x y]}  ^java.util.Map  m
                    [j k _ i]  [1 2 3]]
        (+ a x y))))))


(defn test0  [m v]
  (let [{:keys [x y]}    m
        [a b]            v]
    (+ x y a b)))

;; icfpc.speed> (let [p (->pair 1 2) v [3 4]] (c/quick-bench (test0 p v)))
;; Evaluation count : 8058756 in 6 samples of 1343126 calls.
;;              Execution time mean : 73.013581 ns
;;     Execution time std-deviation : 1.607189 ns
;;    Execution time lower quantile : 71.567415 ns ( 2.5%)
;;    Execution time upper quantile : 75.222269 ns (97.5%)
;;                    Overhead used : 1.794404 ns

(defn test1 [m v]
  (with-slots [{:fields [x y]} ^pair m
               [a b]           ^clojure.lang.PersistentVector v]
    (+ x y a b)))

;; icfpc.speed> (let [p (->pair 1 2) v [3 4]] (c/quick-bench (test1 p v)))
;; Evaluation count : 25859514 in 6 samples of 4309919 calls.
;;              Execution time mean : 21.713334 ns
;;     Execution time std-deviation : 0.562775 ns
;;    Execution time lower quantile : 21.204084 ns ( 2.5%)
;;    Execution time upper quantile : 22.387651 ns (97.5%)
;;                    Overhead used : 1.794404 ns        

)

(comment 
;;becomes with-fields...
(with-fields [{:keys [x y] :as  m} ^pair m]
  (+ x y))

;;becomes with-keys...
(with-fields [{:keys [x y] :as  m} m]
  (+ x y))

;;becomes with-slots
(with-fields [[x y] m]
  (+ x y))

(with-fields [[x y] m]
  (+ x y))
)

;;testing
(comment
  
(defrecord pair [x y])


(defn raw [{:keys [x y] :as m}]
  (+ x y))

;; icfpc.speed> (let [p (->pair 10 20)] (c/quick-bench (raw p)))
;; Evaluation count : 10322586 in 6 samples of 1720431 calls.
;;              Execution time mean : 56.816063 ns
;;     Execution time std-deviation : 0.971773 ns
;;    Execution time lower quantile : 55.210343 ns ( 2.5%)
;;    Execution time upper quantile : 57.832995 ns (97.5%)
;;                    Overhead used : 1.794404 ns


  
(defn flds [m]
  (with-fields ^pair m [x y]
    (+ x y)))

;;6x faster.
;; icfpc.speed> (let [p (->pair 10 20)] (c/quick-bench (flds p)))
;; Evaluation count : 54464514 in 6 samples of 9077419 calls.
;;              Execution time mean : 9.376441 ns
;;     Execution time std-deviation : 0.316239 ns
;;    Execution time lower quantile : 8.950396 ns ( 2.5%)
;;    Execution time upper quantile : 9.688262 ns (97.5%)
;;                    Overhead used : 1.794404 ns
)

(comment
  ;;we'd like something along the following
  ;;transforms...
  (defn blah [{:fields [x y] :as ^level m} k]
    (+ x y k))

  (defn blah [m k]
    (with-fields [^level m [x y]]
      (+ x y k)))

  (defn blah [m k]
    (let [x     (.x ^level m)
          y     (.y ^level m)]
      (+ x y k)))

  ;;we want to do the same thing for indices..
  (defn blah [^Indexed [x y :as xy]]
    (+ x y))

  (defn blah [^Indexed xy]
    (let [x (.nth xy 0)
          y (.nth xy 1)]
      (+ x y)))
)

;;some higher minded reflection stuff...
;;to improve field detection at compile time., maybe later.

;stolen from stack overflow
;; (defn static? [field]
;;   (java.lang.reflect.Modifier/isStatic
;;    (.getModifiers field)))

;; (defmacro record-headers
;;   "Returns a vector of the field names from a record type."
;;   [recname]
;;   (let [rname (symbol (str "->" recname))]
;;   `(vec (map str (first (:arglists (meta #'~rname)))))))

;; (defn get-record-field-names [record]
;;   (->> record
;;        .getDeclaredFields
;;        (remove static?)
;;        (map #(.getName %))
;;        (remove #{"__meta" "__extmap"})))

;;destructuring reproduced from clojure.core...
;;the ideal, language-level solution is to
;;inject field-based destructuring directly
;;and not have to jump through hoops.
;;It looks like we can do that and
;;provide our own let, fn, defn, defmacro
;;forms...that's a bit more invasive and
;;less specialized than I'd like to go at
;;this point, but could be very useful
;;for performance-oriented stuff...


;; icfpc.speed> (#'clojure.core/destructure '[{:keys [x y] :as m} b])
;; [map__8975
;;  b
;;  map__8975
;;  (if
;;   (clojure.core/seq? map__8975)
;;   (clojure.lang.PersistentHashMap/create
;;    (clojure.core/seq map__8975))
;;   map__8975)
;;  m
;;  map__8975
;;  x
;;  (clojure.core/get map__8975 :x)
;;  y
;;  (clojure.core/get map__8975 :y)]
