(ns structural.core
  (:require [clojure.walk :as w]
            [structural.types :as types]
            [clojure.tools.macro :as m]))
;;let's define some macros that let us leverage type hints, unpack
;;destructured vectors, and other goodies that allow us to use extant
;;expressive code, but apply significant optimizations that add up on
;;hot paths....

;;Some basic transforms we'd like to apply in argvectors:

;;[[x y :as xy] ] =>
;; [^clojure.lang.Indexed xy]
;;  (let [x (.nth xy 0)
;;        y (.nth xy 1) ]
;;     ....)

;;we'd like to type hint fields for types and structs...

;;It'd be "really nice" if the type provided could be reflected on so
;;the keys would be automatic field lookups or else delegated to
;;valAt, or in the case of a generic class, fall back to the (get ..)
;;behavior.  Also maybe allow the keys to be hinted?

;;[{:keys [x y] :as ^level m}] =>
;; [^level m]
;;  (let [x (.x m)
;;        y (.y m)]
;;             )

(def warn-on-generic (atom true))

(defn flat-binds [xs]
  (reduce (fn [acc [l r]]
            (conj acc l r))
          [] xs))

(defn can-be? [^Class super ^Class child]
  (.isAssignableFrom super  child))

(defn looks-like? [class obj]
  (or (instance? class obj)
      (can-be? class (type obj))))

(defn field-binds [coll flds]
  (flat-binds
   (for [f flds]
     `[~f (~(symbol (str "." f)) ~coll)])))

(defn slot-getter [class]
  (let [f (cond (or (can-be? clojure.lang.IPersistentVector class)
                    (can-be? clojure.lang.Indexed class))
                '.nth
                (can-be? java.util.List class)
                '.get
                (can-be? clojure.lang.IFn class)
                'invoke
                :else
                'nth)]
    (fn [coll idx]
      `(~f ~coll ~idx))))

(defn key-getter [class]
  (let [f (cond (or (can-be? clojure.lang.IPersistentMap class)
                    (can-be? clojure.lang.IPersistentSet class)
                    (can-be? clojure.lang.Associative class)
                    )
                '.valAt
                (can-be? java.util.Map class)
                '.get
                (can-be? clojure.lang.IFn class)
                'invoke
                :else
                'get)]
    (fn [coll k]
      `(~f ~coll ~k))))

;;add support for
;; (let [{hello :hello blah :world} {:hello "tom" :world 2}]
;;   [hello blah])

(defn slot-binds
  ([coll get-slot flds]
   (let [[xs & [_ [as & rest]]] (partition-by #{:as} flds)]
     (flat-binds
      (concat (for [[idx f] (map-indexed vector xs)
                    :when (not= f '_)]
                `[~f ~(get-slot coll idx)])
              (when as [`[~as ~coll]])))))
  ([coll flds]
   (let [tag (-> coll meta :tag)
         getter (slot-getter (eval (or tag 'Object)))]
     (slot-binds coll getter  flds))))

(defn key-binds
  ([coll get-key flds]
   (flat-binds
    (for [f flds]
      `[~f ~(get-key coll (keyword f))])))
  ([coll flds]
   (let [tag (-> coll meta :tag)
         getter (key-getter (eval (or tag 'Object)))]
     (key-binds coll getter flds))))

(defn ensure-distinct [& ks]
  (let [shared  (map first
                     (filter (fn [[k v]]
                               (> v 1)) (frequencies (apply concat ks))))]
    (when (seq shared)
      (throw (ex-info "duplicate fields or keys detected, check your destructuring form!"
                      {:shared shared })))))

(defn generic-warning [fields coll]
  (let [getter (cond (vector? fields) :nth
                     (map? fields) :get)]
    (println
     [:with-slots.warning/using-generic
      getter
      :ns *ns*
      :fields fields
      :coll   coll
      :try-hinting
      (case getter
        :nth
        '[clojure.lang Indexed IPersistentVector java.util.List]
        :get '[clojure.lang Associative IPersistentMap, java.util.Map])])))

(defn as-binds
  ([fields coll]
   (let [tag    (or (some-> coll meta :tag name symbol)
                    (cond (map? coll)   'clojure.lang.IPersistentMap
                          (vector? coll) 'clojure.lang.IPersistentVector
                          (set? coll)    'clojure.lang.IPersistentSet
                          :else          'Object))
         tagged (with-meta (gensym "coll") {:tag tag})]
     (as-binds tagged fields coll)))
  ([tagged fields coll]
   (let [tag (-> tagged meta :tag)]
     (cond  (= tag 'Object)
            (do (assert (not (:fields fields)) "cannot use :fields key with untyped object!")
                (when @warn-on-generic
                  (generic-warning fields coll))
                `[~fields ~coll])
            ;;maps have associative destructuring...
            (map? fields) ;;{:keys flds :as something}
            (let [flds   (get fields :fields)
                  ks     (get fields :keys) ;;maybe overload this?
                  _      (ensure-distinct flds ks)
                  tgt    (when-let [res (get fields :as)]
                           (if (-> res meta :tag)
                             res
                             (with-meta res {:tag tag})))]
              `[~tagged ~coll
                ~@(into (if tgt `[~tgt ~coll] [])
                        (concat (field-binds tagged flds)
                                (key-binds   tagged ks)))])
            (vector? fields)
            (do (ensure-distinct fields)
                (if (= tagged coll)
                  `[~@(slot-binds tagged fields)]
                  `[~tagged ~coll
                    ~@(slot-binds tagged fields)]))
            :else (throw
                   (ex-info "invalid field spec"
                            {:fields fields :expected [:or :map :vector]}))))))

(defn scrape-symbols [l]
  (cond (symbol? l) l
        (map? l)    (concat (:fields l)
                            (:keys l))
        (vector? l) l
        :else nil))

;;Feels very very dirty using an atom to pass around
;;type information, but it's happening at compile time,
;;and it shouldn't be bad.  Only sfn submits type info,
;;and only with-slots clears it.  Still open to problems
;;though, but it works!
(def init-tags (atom nil))
(defn unify-binds [bindings]
  (let [n       (count bindings)
        _  (assert (and (vector? bindings)
                        (pos? n)
                        (even? n)) "slot bindings must be a vector with an even number of entries!")
        tags (atom (or @init-tags {}))
        get-tag (fn [x]
                  (let [k (name x)]
                    (if-let [t (-> x meta :tag)]
                      (if-not (@tags k)
                        (do (swap! tags assoc k t)
                            t)
                        (do (when (not= t (@tags k))
                              (println [:with-slots.warning/changed-tags
                                        :symbol x :tag t :previously (@tags k)]))
                            t))
                      (@tags k))))
        tag (fn [x]
              (if-let [t (get-tag x)]
                (with-meta x {:tag t})
                x))
        tag-bind (fn [[l r]]
                   (let [scrape? (or (map? l) (vector? l))
                         xs (when scrape? (mapv tag (scrape-symbols l)))]
                     [l (if (symbol? r) (tag r) r)]))
        n (atom 0)
        res  (apply concat (for [b (partition 2 bindings)]
                             (let [[l r] (tag-bind b)
                                   re-use? (and (pos? @n)
                                                (symbol? r))
                                   _ (swap! n inc)]
                               (cond (symbol? l) [l r]
                                     re-use?     (as-binds r l r)
                                     :else       (as-binds l r)))))]
    res))

(def coll-re #"coll[0-9]+")
(defn coll-sym? [x]
  (when (symbol? x)
    (re-find coll-re  (name x))))

;;we need to establish a final pass to eliminate needless bindings,
;;since bindings still cost...

;;given
;; [^clojure.lang.Indexed coll14941
;;  ^clojure.lang.Indexed assign-result
;;  board
;;  (.nth ^clojure.lang.Indexed coll14941 0)
;;  aff-indexes
;;  (.nth ^clojure.lang.Indexed coll14941 1)]

(defn dupe? [l r]
  (and (symbol? l) (symbol? r)
       (= (name l) (name r))
       (= (-> l meta :tag) (-> r meta :tag))))

(defn collapse-binds [xs]
  (let [replacements (atom {})]
    (reduce (fn [acc [l r]]
              (cond (and (coll-sym? l) (symbol? r))
                      (do (swap! replacements assoc l r)
                          acc)
                    (dupe? l r)
                      acc
                    :else
                      (conj acc l (w/postwalk-replace @replacements r))))
            []
            (partition 2 xs))))

(defmacro with-slots
  "Allows for efficient, type-based destructuring similar to the
  idiomatic destructuring forms of Clojure, with some limitations.
  Bindings are presented as the typical vector, with an even number of
  entries, where the preceding odd binding establishes binds for the
  even successor.  Unlike typical forms, bindings leverage
  type-hinting information - both on the left hand side and the right
  hand side - to establish efficient operations beyond the generic
  destructuring forms established with maps and vectors, e.g. get and
  nth.

  Callers may use {:fields [a b ^clojure.lang.Counted c] }, along with
  a type-hinted rhs, to denote establishing bindings for a, b, c, by
  invoking like-named direct, type-hinted field applications on the
  rhs, ala (.a ^some-type rhs).

  Any binding var hinted on the LHS will propogate its hint throughout
  later bindings.  This allows an expressive form of efficient
  destructuring for the consenting adult, which allows idiomatic
  expressivity without the accompanying significant loss of
  performance.

  map destructuring for {:keys [...]} follows that of :fields, except
  the bindings are established via either a (.valAt ..) or (.get ..)
  or (get ...) depending on the presented type, get being the fallback.
  This allows usage with types supporting the java.util.Map interface.
  Literal maps are automatically inferred with efficient getters.

  Vector or indexed destructuring is similarly supported,
  [^some-type x y] ^clojure.lang.Indexed coll will invoke efficient
  .nth indexing operations rather than the slower, more general nth.
  Depending on the presented type, either .nth, .get, or nth will be
  used, allowing operation with structures supporting the
  java.util.List interface.  Literal vectors are automatically
  inferred with efficient getters.  The & rest notation is currently
  NOT supported...

  The remaining rules act identically to let semantics.  If a symbol
  is bound to the LHS, then the binding is passed through
  untouched (including hints).

  with-slots tries to scan the input bindings to find
  discrepancies (such as duplicate binds), and to re-use existing
  hinted information for binds.  In the case that the user decides to
  re-hint a RHS var that has already been hinted a-priori, with-slots
  will allow the hint for that binding, but revert to prior hinting
  unless the user continues to specify new hints.  This seems rare in
  practice.

  It's common to import the symbols for the
  [clojure.lang Counted Indexed] interfaces when using with-slots.

  An example:

  (with-slots
    [{:fields [^Counted path
               ^Indexed position]} ^botmove (->botmove [] [1 2])
     {:keys [a b] :fields [hashCode]}    {:a 2 :b 3}
     [x y]          position
     path-length   (.count path)]
    [hashCode (+ x y)])
  "
  [bindings & body]
  (let [[_ bindings & body] &form
        new-bindings (collapse-binds (unify-binds bindings))
        ;;sorry for the state...
        _ (reset! init-tags {})]
    `(let [~@new-bindings]
       ~@body)))

#_
(defn blah [{:keys [x y]} ])

#_
(s/fn [^Indexed [dx' dy']]
  (obstacle? level x y dx' dy'))

#_
(fn [dxdy]
  (with-slots [[dx' dy'] ^Indexed dxdy]
    (obstacle? level x y dx' dy')))


;;infer from the record type would be nice...
#_
(s/defn bot-covering [{:fields [^Indexed bots bot] :as ^lev level}]
  (with-slots
    [{:keys [^long x ^long y layout]}            ^IPersistentMap (.nth bots bot)]
    (eduction  (map (fn [dxdy] 
                      (with-slots [[^long dx ^long dy] ^Indexed dxdy]
                        (when (or (and (zero? dx) (zero? dy)
                                       (valid? x y level))
                                  (valid-hand? x y dx dy level))
                          [(+ x dx) (+ y dy)]))))
               (filter identity) layout)))

#_
(defn bot-covering [level]
  (with-slots
    [{:fields [^Indexed bots bot]}   ^lev level
     {:keys [^long x ^long y layout]}            ^IPersistentMap (.nth bots bot)]
    (eduction  (map (fn [dxdy] 
                      (with-slots [[^long dx ^long dy] ^Indexed dxdy]
                        (when (or (and (zero? dx) (zero? dy)
                                       (valid? x y level))
                                  (valid-hand? x y dx dy level))
                          [(+ x dx) (+ y dy)]))))
               (filter identity) layout)))


;;basic transform of
#_
(fn [[unwrapping :as ^hint wrapped]] ;;no :as defined, create :as...
  (with-slots [more-unrwapping]
    ...))

#_
(fn [wrapped]
  (with-slots [unwrapping ^hint wrapped
               more-unwrapping ...]))

;;clojure.core/maybe-destructured, EPL v 1.0
#_
(defn  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

;;let-forms should be expanded to with-slots.
(defn sfn->fn [nm args & body]
  (let [head    (first args)]
    (if-not (or (map? head) (vector? head))
      ;;don't bother destructuring sequences for now.
      ;;need to propogate the function arg type though,
      ;;so that with-slots can pick up on it.
      (-> `(fn ~args ~@body)
          (with-meta :init-tags
            (if  head
              {(name head) (-> head meta :tag)}
              {})))
      (let [t       (-> head meta :tag)
            as-tgt  (if (map? head)
                      (head :as)
                      (let [tgt (some-> (partition-by #{:as} args)
                                        vec
                                        (nth 2 nil))
                            _   (assert (if tgt  (= (count tgt) 1) true)
                                        "only one symbol allowed for :as in vector!")]
                        (first tgt)))
            as?      (some? as-tgt)
            as-tgt   (with-meta (or as-tgt (gensym "arg")) {:tag t})
            non-as-binding (if (map? head)
                             (dissoc head :as)
                             (filterv (complement #{:as as-tgt}) head))]
        `(fn ~nm [~as-tgt]
           (structural.core/with-slots [~non-as-binding ~as-tgt]
             ~@body))))))

(defmacro sfn
  "Like clojure.core/fn, except recognizes structural bindings akin to
   structural.core/with-slots, and treats the arg vector as a structural
   binding form as well."
  [name args & body]
  (let [[_ name args & body] &form
        t (-> args first meta :tag)
        f (gensym "form")
        b (gensym "body")
        expr (apply sfn->fn name args body)
        _    (reset! init-tags (or (-> expr meta :init-tags) {}))]
    `(m/macrolet [(~'let [~f ~'& ~b]
                   (list* ~''structural.core/with-slots ~f ~b))]
        ~expr)))

(defmacro sdefn
  "Like clojure.core/fn, except recognizes structural bindings akin to
   structural.core/with-slots, and treats the arg vector as a structural
   binding form as well."
  [name args & body]
  (let [[_ name args & body] &form])
  `(def ~name
     (sfn ~name ~args ~@body)))


