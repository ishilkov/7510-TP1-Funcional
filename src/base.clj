(ns base)

(def rules {})
(def facts {})

(defn get-rule
  [rules prop]
  (get rules (get prop :rule))
  )

(defn get-fact
  [facts prop]
  (get facts (get prop :rule))
  )

(defn build-atom
  [entry atom-type value-type]
  (let [matches (re-matches #"\s*(\w+)\s*\((\s*\w+\s*[,\s*\w+\s*]*)\)\s*"  entry)
        ]
    (if (nil? matches)
      (throw (ex-info "Incorrect lexical definition" {:type :lexical-definition}))
      (let [prop (zipmap [:expr atom-type value-type] matches)]
        {atom-type (get prop atom-type), value-type (re-seq #"\w+" (get prop value-type))}
        )
      )
    )
  )

(defn build-proposition
  [prop]
  (build-atom prop :rule :vals)
  )

(defn build-conditions
  [entry]
  (let [cond-exp (re-seq #"\s*\w+\s*\(\s*\w+\s*[,\s*\w\s*]*\)" entry)]
    (map #(build-atom %1 :fact :temp) cond-exp)
    )
  )

(defn build-rule
  [entry]
  (let [rule (zipmap [:expr :prop :conds] (re-matches #"(.*):-(.*)" entry))
        prop (get rule :prop)
        conds (get rule :conds)
        ]
    {:prop (build-atom prop :rule :temp), :conds (build-conditions conds)}
    )
  )

(defn build-entry
  [entry]
  (if-not (nil? (re-matches #".*:-.*" entry))
    (let [rule (build-rule entry)
          key (get (get rule :prop) :rule)]
      (def rules (merge rules {key rule}))
      )
    (let [fact (build-atom entry :fact :vals)
          key (get fact :fact)
          vals (get fact :vals)]
      (def facts (assoc facts key (conj (get facts key) vals)))

      )
    )
  )

(defn build-base-from-parsed
  [entries]
  (doseq [entry entries]
     (build-entry entry)
     )
    {:rules rules, :facts facts}
  )

(defn build-base
  [database]
  (build-base-from-parsed (clojure.string/split
                            (clojure.string/replace database
                                                    #"[\t\n]"
                                                    "")
                            #"\.")))





