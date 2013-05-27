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
  {:in [:integer :integer]
   :out [:integer]}
  (adder :integer))
(define-registered float_add
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (subtracter :integer))
(define-registered float_sub
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (multiplier :integer))
(define-registered float_mult
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (divider :integer))
(define-registered float_div
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (modder :integer))
(define-registered float_mod
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:boolean]}
  (lessthaner :integer))
(define-registered float_lt
  {:in [:float :float]
   :out [:boolean]}
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
  {:in [:integer :integer]
   :out [:boolean]}
  (greaterthaner :integer))
(define-registered float_gt
  {:in [:float :float]
   :out [:boolean]}
  (greaterthaner :float))

(define-registered 
  integer_fromboolean
  {:in [:boolean]
   :out [:integer]}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1 0) :integer)))
      state)))

(define-registered 
  float_fromboolean
  {:in [:boolean]
   :out [:float]}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1.0 0.0) :float)))
      state)))

(define-registered 
  integer_fromfloat
  {:in [:float]
   :out [:integer]}
  (fn [state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (truncate item) :integer)))
      state)))

(define-registered 
  float_frominteger
  {:in [:integer]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (minner :integer))
(define-registered float_min
  {:in [:float :float]
   :out [:float]}
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
  {:in [:integer :integer]
   :out [:integer]}
  (maxer :integer))
(define-registered float_max
  {:in [:float :float]
   :out [:float]}
  (maxer :float))

(define-registered 
  float_sin
  {:in [:float]
   :out [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/sin (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered 
  float_cos
  {:in [:float]
   :out [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/cos (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered 
  float_tan
  {:in [:float]
   :out [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/tan (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))
