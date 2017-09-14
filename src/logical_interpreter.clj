(ns logical-interpreter
  (:require base))

(defn map-proposition
  [rule prop]
  (let [propVals (get prop :vals)
        propTemp (get rule :prop)]
    (apply zipmap [(get propTemp :temp) propVals])
    )
  )

(defn map-condition
  [cond propMap]
  (let [condTemp (get cond :temp)
        condVals (apply map [propMap condTemp])]
    (apply zipmap [condTemp condVals])
    )
  )

(defn evaluate-condition
  [facts cond condMap]
  (some #(= (vals condMap) %) (get facts (get cond :fact)))
  )


(defn evaluate-proposition
  [facts rules prop]
  (let [rule (base/get-rule rules prop)
        propMap (map-proposition rule prop)
        conds (get rule :conds)
        mapConds (map #(map-condition %1 propMap) conds)
        ]
    (every? true? (map #(evaluate-condition facts %1 %2) conds mapConds))
    )
  )


(def database ["varon(jose)"
               "varon(pepe)"
               "varon(marcos)"
               "mujer(vero)"
               "mujer(maria)"
               "padre(jose,marcos)"
               "padre(jose,pepe)"
               "padre(jose,vero)"
               "hijo(X,Y):-varon(X),padre(Y,X)"
               "hija(X,Y):-mujer(X),padre(Y,X)"
               ])

(defn evaluate-query
  [database query]
  (let [base (base/build-base database)]
    (evaluate-proposition (get base :facts)
                          (get base :rules)
                          (base/build-proposition query))
    )

  )

(println (evaluate-query database  "hijo(pepe,jose)"))
(println (evaluate-query database  "hijo(jose,pepe)"))

