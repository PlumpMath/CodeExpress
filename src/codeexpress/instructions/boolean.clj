(ns codeexpress.instructions.boolean
  (:use [codeexpress.pushstate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for Booleans

(define-registered
  boolean_and
  {:in {:boolean 2} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (and (stack-ref :boolean 0 state)
                      (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_or
  {:in {:boolean 2} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (or (stack-ref :boolean 0 state)
                     (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_not
  {:in {:boolean 1} 
   :out {:boolean 1}}
  (fn 
    [state]
    (if (not (empty? (:boolean state)))
      (push-item (not (stack-ref :boolean 0 state))
                 :boolean
                 (pop-item :boolean state))
      state)))

(define-registered
  boolean_xor
  {:in {:boolean 2} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (not= (stack-ref :boolean 0 state)
                       (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_invert_first_then_and
  {:in {:boolean 2} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (and (not (stack-ref :boolean 0 state))
                      (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_invert_second_then_and
  {:in {:boolean 2} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (and (stack-ref :boolean 0 state)
                      (not (stack-ref :boolean 1 state)))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_frominteger
  {:in {:integer 1} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (not (zero? (stack-ref :integer 0 state)))
                 :boolean
                 (pop-item :integer state))
      state)))

(define-registered
  boolean_fromfloat
  {:in {:float 1} 
   :out {:boolean 1}}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (not (zero? (stack-ref :float 0 state)))
                 :boolean
                 (pop-item :float state))
      state)))
