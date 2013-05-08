(ns codeexpress.instructions.numbers
  (:use [codeexpress.pushstate]
        [codeexpress.util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for numbers

(defn adder
  "Returns a function that pushes the sum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (+' first second)) type)))
      state)))

(define-registered integer_add 
  {:in {:integer 2}
   :out {:integer 1}}
  (adder :integer))
(define-registered float_add
  {:in {:float 2}
   :out {:float 1}}
  (adder :float))

(defn subtracter
  "Returns a function that pushes the difference of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (- second first)) type)))
      state)))

(define-registered integer_sub
  {:in {:integer 2}
   :out {:integer 1}}
  (subtracter :integer))
(define-registered float_sub
  {:in {:float 2}
   :out {:float 1}}
  (subtracter :float))

(defn multiplier
  "Returns a function that pushes the product of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (*' second first)) type)))
      state)))

(define-registered integer_mult
  {:in {:integer 2}
   :out {:integer 1}}
  (multiplier :integer))
(define-registered float_mult
  {:in {:float 2}
   :out {:float 1}}
  (multiplier :float))

(defn divider
  "Returns a function that pushes the quotient of the top two items. Does
   nothing if the denominator would be zero."
  [type]
  (fn [state]
    (if (and (not (empty? (rest (type state))))
             (not (zero? (stack-ref type 0 state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (if (= type :integer)
                          (truncate (keep-number-reasonable (/ second first)))
                          (keep-number-reasonable (/ second first)))
                        type)))
      state)))

(define-registered integer_div
  {:in {:integer 2}
   :out {:integer 1}}
  (divider :integer))
(define-registered float_div
  {:in {:float 2}
   :out {:float 1}}
  (divider :float))

(defn modder
  "Returns a function that pushes the modulus of the top two items. Does
   nothing if the denominator would be zero."
  [type]
  (fn [state]
    (if (and (not (empty? (rest (type state))))
             (not (zero? (stack-ref type 0 state))))
      (let [frst (stack-ref type 0 state)
            scnd (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (if (= type :integer)
                          (truncate (keep-number-reasonable (mod scnd frst)))
                          (keep-number-reasonable (mod scnd frst)))
                        type)))
      state)))

(define-registered integer_mod
  {:in {:integer 2}
   :out {:integer 1}}
  (modder :integer))
(define-registered float_mod
  {:in {:float 2}
   :out {:float 1}}
  (modder :float))

(defn lessthaner
  "Returns a function that pushes the result of < of the top two items onto the 
   boolean stack."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (< second first) :boolean)))
      state)))

(define-registered integer_lt
  {:in {:integer 2}
   :out {:boolean 1}}
  (lessthaner :integer))
(define-registered float_lt
  {:in {:float 2}
   :out {:boolean 1}}
  (lessthaner :float))

(defn greaterthaner
  "Returns a function that pushes the result of > of the top two items onto the 
   boolean stack."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (> second first) :boolean)))
      state)))

(define-registered integer_gt
  {:in {:integer 2}
   :out {:boolean 1}}
  (greaterthaner :integer))
(define-registered float_gt
  {:in {:float 2}
   :out {:boolean 1}}
  (greaterthaner :float))

(define-registered 
  integer_fromboolean
  {:in {:boolean 1}
   :out {:integer 1}}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1 0) :integer)))
      state)))

(define-registered 
  float_fromboolean
  {:in {:boolean 1}
   :out {:float 1}}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1.0 0.0) :float)))
      state)))

(define-registered 
  integer_fromfloat
  {:in {:float 1}
   :out {:integer 1}}
  (fn [state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (truncate item) :integer)))
      state)))

(define-registered 
  float_frominteger
  {:in {:integer 1}
   :out {:float 1}}
  (fn [state]
    (if (not (empty? (:integer state)))
      (let [item (stack-ref :integer 0 state)]
        (->> (pop-item :integer state)
             (push-item (*' 1.0 item) :float)))
      state)))

(defn minner
  "Returns a function that pushes the minimum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (min second first) type)))
      state)))

(define-registered integer_min
  {:in {:integer 2}
   :out {:integer 1}}
  (minner :integer))
(define-registered float_min
  {:in {:float 2}
   :out {:float 1}}
  (minner :float))

(defn maxer
  "Returns a function that pushes the maximum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (max second first) type)))
      state)))

(define-registered integer_max
  {:in {:integer 2}
   :out {:integer 1}}
  (maxer :integer))
(define-registered float_max
  {:in {:float 2}
   :out {:float 1}}
  (maxer :float))

(define-registered 
  float_sin
  {:in {:float 1}
   :out {:float 1}}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/sin (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered 
  float_cos
  {:in {:float 1}
   :out {:float 1}}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/cos (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered 
  float_tan
  {:in {:float 1}
   :out {:float 1}}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/tan (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))
