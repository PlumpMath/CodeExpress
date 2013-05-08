(ns codeexpress.prefix.trees
  (:use [codeexpress.globals]))

(defn instruction-type
  "What type does this instruction return?"
  [inst]
  (first (keys (get @specification-table inst))))

(defn random-tree
  "Generate a random tree. This should maintain typing."
  [type depth-limit method] ;; method should be :grow or :full
  (if (or (= depth-limit 0)
          (and (= method :grow)
               (< (rand) (:terminal-proportion @code-parms))))
    (expand-instruction (lrand-nth (get (:terminals @code-parms) type)))
    (let [f (expand-instruction (lrand-nth (get (:functions @code-parms) type)))
          args (arg-types f)]
      (cons f (map (fn [type]
                     (random-tree type (dec depth-limit) method)) args)))))

  
