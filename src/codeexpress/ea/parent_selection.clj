(ns codeexpress.ea.parent-selection
  (:use [codeexpress.random]
        [codeexpress.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent, using lexicase or tournament selection."
  [pop tournament-size radius location]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
                 (if (zero? radius)
                   (lrand-int (count pop))
                   (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                        (count pop))))))
        err-fn (cond
                 @global-use-historically-assessed-hardness :hah-error
                 @global-use-rmse :rms-error
                 true :total-error)]
    (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
            tournament-set)))
