(ns codeexpress.ea.breed
  (:use [codeexpress.globals]
        [codeexpress.random]
        [codeexpress.ea.parent-selection]
        [codeexpress.ea.genetic-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn breed
  "Replaces the state of the given agent with an individual bred from the given population (pop), 
   using the given parameters."
  [agt location rand-gen pop
   {:keys [error-function max-points atom-generators 
           mutation-probability mutation-max-points crossover-probability 
           tournament-size trivial-geography-radius]}]
  (binding [*thread-local-random-generator* rand-gen]
    (let [n (lrand)]
      (cond 
        ;; mutation
        (< n mutation-probability)
        (let [parent (select pop tournament-size trivial-geography-radius location)]
          (assoc (mutate parent mutation-max-points max-points atom-generators) :parent parent))
        ;; crossover
        (< n (+ mutation-probability crossover-probability))
        (let [first-parent (select pop tournament-size trivial-geography-radius location)
              second-parent (select pop tournament-size trivial-geography-radius location)]
          (assoc (crossover first-parent second-parent max-points) :parent first-parent))
        ;; replication
        true 
        (let [parent (select pop tournament-size trivial-geography-radius location)]
          (assoc parent :parent parent))))))
