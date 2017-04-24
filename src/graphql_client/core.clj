(ns graphql-client.core)

(declare args->str)

(defn arg->str
  ([v] (arg->str name v))
  ([kw-xform v]
   (cond (map? v) (str "{" (args->str kw-xform v) "}")
         (keyword? v) (str "\"" (kw-xform v) "\"")
         (string? v) (str "\"" v "\"")
         (coll? v) (str "[" (apply str (interpose "," (map (partial arg->str kw-xform) v))) "]")
         :else (str v))))

(defn args->str
  ([m] (args->str name m))
  ([kw-xform m]
   (->> (for [[k v] m]
          [(kw-xform k) ":" (arg->str kw-xform v)])
        (interpose ",")
        (flatten)
        (apply str))))

(defn query->str
  ([q] (query->str name q))
  ([kw-xform q]
   (if (keyword? q)
     (kw-xform q)
     (let [f (first q)
           s (second q)
           r (if (map? s) (drop 2 q) (rest q))]
       (apply str (flatten (cond (or (keyword? f)
                                     (map? f)) [(if (map? f)
                                                  [(kw-xform (ffirst f)) ":" (kw-xform (second (first f)))]
                                                  (kw-xform f))
                                                (if (map? s) ["(" (args->str kw-xform s) ")"] "")
                                                (when (seq r) ["{" (interpose " " (map (partial query->str kw-xform) r)) "}"])]
                                 (vector? f) ["{" (map (partial query->str kw-xform) q) "}"]
                                 :else "")))
       ))))
