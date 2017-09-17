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
  (some? (some #(= (vals condMap) %) (get facts (get cond :fact))))
  )

(defn evaluate-fact
  [facts fact]
  (some? (some #(= (get fact :vals) %) (get facts (get fact :rule))))
  )

(defn evaluate-proposition
  [facts rules prop]
  (let [rule (base/get-rule rules prop)
        propMap (map-proposition rule prop)
        conds (get rule :conds)
        mapConds (map #(map-condition %1 propMap) conds)
        ]
    (and (not (nil? rule))
         (every? true? (map #(evaluate-condition facts %1 %2) conds mapConds)))

    )
  )

(defn evaluate-query
  [database query]
  (try
    (let [base (base/build-base database)
          prop (base/build-proposition query)
          facts (get base :facts)
          rules (get base :rules)
          ]
       (if (not (nil? (base/get-fact facts prop)))
         (evaluate-fact facts
                        prop)
         (evaluate-proposition facts
                               rules
                               prop)
         )
       )
    (catch Exception e (ex-data e)
      (if (= :lexical-definition (-> e :type)) nil)
      )
    )
  )

(def database "
  varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan,pepe).
	padre(juan,pepa).
	padre(hector,maria).
	padre(roberto,alejandro).
	padre(roberto,cecilia).
	hijo(X,Y):-varon(X),padre(Y,X).
	hija(X,Y):-mujer(X),padre(Y,X).
	")

;(println (evaluate-query database "varon (pepe) "))
;(println (evaluate-query database "varonn(jose)"))
;(println (evaluate-query database "varon ( jose)"))
;(println (evaluate-query database "hijoo(pepe, jose)"))
;(println (evaluate-query database "hijo(pepe, juan )"))
;(println (evaluate-query database "hijo( juan , pepe )"))

