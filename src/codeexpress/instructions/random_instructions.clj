(ns codeexpress.instructions.random-instructions
  (:use [codeexpress.pushstate]
        [codeexpress.random]
        [codeexpress.globals])
  (:require [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random instructions

(define-registered
  boolean_rand
  {:in {}
   :out {:boolean 1}}
  (fn [state]
    (push-item (lrand-nth [true false]) :boolean state)))

(define-registered
  integer_rand
  {:in {}
   :out {:integer 1}}
  (fn [state]
    (push-item (+' (lrand-int (+ 1 (- max-random-integer min-random-integer)))
                   min-random-integer)
               :integer
               state)))

(define-registered
  float_rand
  {:in {}
   :out {:float 1}}
  (fn [state]
    (push-item (+' (lrand (- max-random-float min-random-float))
                   min-random-float)
               :float
               state)))

(define-registered
  code_rand
  {:in {:integer 1}
   :out {:code 1}}
  (fn [state]
    (if (not (empty? (:integer state)))
      (if (empty? @codeexpress.globals/global-atom-generators)
	(binding [*out* *err*]
	  (println "code_rand: global-atom-generators is empty.")
	  state)
	(push-item (random-code (math/abs (mod (stack-ref :integer 0 state)
					       max-points-in-random-expressions))
				@global-atom-generators)
		   :code
		   (pop-item :integer state)))
      state)))

(define-registered
  string_rand
  {:in {}
   :out {:string 1}}
  (fn [state]
    (push-item
      (apply str (repeatedly
                   (+' min-random-string-length
                       (lrand-int (- max-random-string-length
                                     min-random-string-length)))
                   #(rand-nth
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")))
      :string
      state)))
