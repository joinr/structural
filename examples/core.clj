(ns examples.core
  (:require [structural.core :as s]
            [criterium.core :as c])
  (:import [clojure.lang Indexed Counted IPersistentMap]))

(defmacro with-unchecked [& body]
  `(do (set! *unchecked-math* true)
       (let [res# ~@body]
         (set! *unchecked-math* false)
         res#)))

;;this will illicit a warning
(s/sdefn add-vec [[^long x ^long y]]
  (+ x y))

;; [:with-slots.warning/using-generic :nth :ns
;; #namespace[examples.core] :fields [x y] :coll
;; arg16626 :try-hinting [clojure.lang Indexed IPersistentVector
;; java.util.List]]

(s/sdefn add-vec-faster
       [^clojure.lang.Indexed [^long x ^long y]]
       (+ x y))


;;~5.85x faster
;; examples.core> (let [v [1 2]] (c/quick-bench (add-vec v)))
;; Evaluation count : 15366258 in 6 samples of 2561043 calls.
;; Execution time mean : 41.275964 ns
;; Execution time std-deviation : 1.981333 ns
;; Execution time lower quantile : 39.431612 ns ( 2.5%)
;; Execution time upper quantile : 43.776804 ns (97.5%)
;; Overhead used : 1.818301 ns
;; nil


;; examples.core> (let [v [1 2]] (c/quick-bench (add-vec-faster v)))
;; Evaluation count : 67154058 in 6 samples of 11192343 calls.
;; Execution time mean : 7.012811 ns
;; Execution time std-deviation : 0.231733 ns
;; Execution time lower quantile : 6.784277 ns ( 2.5%)
;; Execution time upper quantile : 7.356227 ns (97.5%)
;; Overhead used : 1.818301 ns
;; nil

(defprotocol IBlah
  (blah [this]))

(defrecord point [^long x ^long y]
  IBlah
  (blah [_] (+ x y)))

(s/with-slots [[^IPersistentMap m ^Indexed v ^point p]  [{:a 2 :b 3} [4 5 6] (->point 1 2)]
               {:fields [count] :keys [a b]}       m
               [c d e]                             v
               {:fields [x y blah]}                p]
  (+ a b c d e blah count))

(defn test-point [{:keys [x y] :as p}]
  (== (+ x y) (blah p)))

(s/sdefn test-point-faster [^point {:fields [x y blah] :as p}]
         (== (+ x y) blah))

(def p (->point 1 2))
(= (test-point p) (test-point-faster p))
;;true

;;Field access is very nice.

;;~9x faster...

;; examples.core> (c/quick-bench (test-point p))
;; Evaluation count : 8458806 in 6 samples of 1409801 calls.
;; Execution time mean : 72.621662 ns
;; Execution time std-deviation : 2.641677 ns
;; Execution time lower quantile : 70.122846 ns ( 2.5%)
;; Execution time upper quantile : 75.570272 ns (97.5%)
;; Overhead used : 1.818301 ns
;; nil

;; examples.core> (c/quick-bench (test-point-faster p))
;; Evaluation count : 58061634 in 6 samples of 9676939 calls.
;; Execution time mean : 8.443256 ns
;; Execution time std-deviation : 0.285232 ns
;; Execution time lower quantile : 8.023582 ns ( 2.5%)
;; Execution time upper quantile : 8.744502 ns (97.5%)
;; Overhead used : 1.818301 ns
;; nil
