;; tg8.clj
;; an example problem for codeexpress, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns codeexpress.examples.pushgp.tg8
  (:use [codeexpress.pushgp.pushgp]
        [codeexpress.pushstate]
        [codeexpress.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Integer symbolic regression of the y=5x^6-2x^5-5x^3+3x^2+5. This uses
;; the core integer arithmetic instructions and an input instruction that uses the auxiliary stack.
;; This example also demonstrates  the use of fitness penalties for abnormally terminating programs.
;; This is example 8 from the "trivial geography" chapter, 
;; http://hampshire.edu/lspector/pubs/trivial-geography-toappear.pdf
;; (Hence the name tg8.)


(define-registered in 
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(def argmap
  {:error-function (fn [program]
                     (doall
                       (for [input (range 10)]
                         (let [state (run-push program 
                                               (push-item input :auxiliary 
                                                          (push-item input :integer
                                                                     (make-push-state))))
                               top-int (top-item :integer state)
                               invalid-output (or (not (number? top-int))
                                                  (= (:termination state) :abnormal))]
                           (if invalid-output
                             10000000
                             (abs (- top-int
                                     (+ (* 5 input input input input input input)
                                        (* -2 input input input input input)
                                        (* -5 input input input)
                                        (* 3 input input)
                                        5))))))))
   :atom-generators (concat 
                      '(integer_add integer_sub integer_mult integer_div)
                      (list 
                        (fn [] (- (rand-int 21) 10))
                        'in))
   :mutation-probability 0.3
   :crossover-probability 0.3
   :simplification-probability 0.3
   :reproduction-simplifications 10
   :max-points 100
   :max-points-in-initial-program 100
   :population-size 5000
   })
