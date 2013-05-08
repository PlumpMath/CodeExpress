(ns codeexpress.instructions.common
  (:use [codeexpress.pushstate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for all types (except auxiliary and tag)

(defn popper 
  "Returns a function that takes a state and pops the appropriate stack of the state."
  [type]
  (fn [state] (pop-item type state)))

(define-registered exec_pop
  {:out {:exec -1}}
  (popper :exec))
(define-registered integer_pop
  {:out {:integer -1}}
  (popper :integer))
(define-registered float_pop
  {:out {:float -1}}
  (popper :float))
(define-registered code_pop
  {:out {:code -1}}
  (popper :code))
(define-registered boolean_pop
  {:out {:boolean -1}}
  (popper :boolean))
(define-registered zip_pop
  {:out {:zip -1}}
  (popper :zip))
(define-registered string_pop
  {:out {:string -1}}
  (popper :string))

(defn duper 
  "Returns a function that takes a state and duplicates the top item of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (push-item (top-item type state) type state))))

(define-registered exec_dup
  {:in {:exec 1}
   :out {:exec 2}}
  (duper :exec))
(define-registered integer_dup
  {:in {:integer 1}
   :out {:integer 2}}
  (duper :integer))
(define-registered float_dup
  {:in {:float 1}
   :out {:float 2}}
  (duper :float))
(define-registered code_dup
  {:in {:code 1}
   :out {:code 2}}
  (duper :code))
(define-registered boolean_dup
  {:in {:boolean 1}
   :out {:boolean 2}}
  (duper :boolean))
(define-registered zip_dup
  {:in {:zip 1}
   :out {:zip 2}}
  (duper :zip))
(define-registered string_dup
  {:in {:string 1}
   :out {:string 2}}
  (duper :string))

(defn swapper 
  "Returns a function that takes a state and swaps the top 2 items of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first-item (stack-ref type 0 state)
            second-item (stack-ref type 1 state)]
        (->> (pop-item type state) 
             (pop-item type)
             (push-item first-item type)
             (push-item second-item type)))
      state)))

(define-registered exec_swap {} (swapper :exec))
(define-registered integer_swap {} (swapper :integer))
(define-registered float_swap {} (swapper :float))
(define-registered code_swap {} (swapper :code))
(define-registered boolean_swap {} (swapper :boolean))
(define-registered zip_swap {} (swapper :zip))
(define-registered string_swap {} (swapper :string))

(defn rotter 
  "Returns a function that takes a state and rotates the top 3 items of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (not (empty? (rest (rest (type state)))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)
            third (stack-ref type 2 state)]
        (->> (pop-item type state)
             (pop-item type)
             (pop-item type)
             (push-item second type)
             (push-item first type)
             (push-item third type)))
      state)))

(define-registered exec_rot {} (rotter :exec))
(define-registered integer_rot {} (rotter :integer))
(define-registered float_rot {} (rotter :float))
(define-registered code_rot {} (rotter :code))
(define-registered boolean_rot {} (rotter :boolean))
(define-registered zip_rot {} (rotter :zip))
(define-registered string_rot {} (rotter :string))

(defn flusher
  "Returns a function that empties the stack of the given state."
  [type]
  (fn [state]
    (assoc state type '())))

(define-registered exec_flush {} (flusher :exec))
(define-registered integer_flush {} (flusher :integer))
(define-registered float_flush {} (flusher :float))
(define-registered code_flush {} (flusher :code))
(define-registered boolean_flush {} (flusher :boolean))
(define-registered zip_flush {} (flusher :zip))
(define-registered string_flush {} (flusher :string))


(defn eqer 
  "Returns a function that compares the top two items of the appropriate stack of 
   the given state."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (= first second) :boolean)))
      state)))

(define-registered exec_eq {} (eqer :exec))
(define-registered integer_eq {} (eqer :integer))
(define-registered float_eq {} (eqer :float))
(define-registered code_eq {} (eqer :code))
(define-registered boolean_eq {} (eqer :boolean))
(define-registered zip_eq {} (eqer :zip))
(define-registered string_eq {} (eqer :string))

(defn stackdepther
  "Returns a function that pushes the depth of the appropriate stack of the 
   given state."
  [type]
  (fn [state]
    (push-item (count (type state)) :integer state)))

(define-registered exec_stackdepth {} (stackdepther :exec))
(define-registered integer_stackdepth {} (stackdepther :integer))
(define-registered float_stackdepth {} (stackdepther :float))
(define-registered code_stackdepth {} (stackdepther :code))
(define-registered boolean_stackdepth {} (stackdepther :boolean))
(define-registered zip_stackdepth {} (stackdepther :zip))
(define-registered string_stackdepth {} (stackdepther :string))

(defn yanker
  "Returns a function that yanks an item from deep in the specified stack,
   using the top integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            actual-index (max 0 (min raw-index (- (count (type with-index-popped)) 1)))
            item (stack-ref type actual-index with-index-popped)
            with-item-pulled (assoc with-index-popped 
                                    type 
                                    (let [stk (type with-index-popped)]
                                      (concat (take actual-index stk)
                                              (rest (drop actual-index stk)))))]
        (push-item item type with-item-pulled))
      state)))

(define-registered exec_yank {} (yanker :exec))
(define-registered integer_yank {} (yanker :integer))
(define-registered float_yank {} (yanker :float))
(define-registered code_yank {} (yanker :code))
(define-registered boolean_yank {} (yanker :boolean))
(define-registered zip_yank {} (yanker :zip))
(define-registered string_yank {} (yanker :string))

(defn yankduper
  "Returns a function that yanks a copy of an item from deep in the specified stack,
   using the top integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            actual-index (max 0 (min raw-index (- (count (type with-index-popped)) 1)))
            item (stack-ref type actual-index with-index-popped)]
        (push-item item type with-index-popped))
      state)))

(define-registered exec_yankdup {} (yankduper :exec))
(define-registered integer_yankdup {} (yankduper :integer))
(define-registered float_yankdup {} (yankduper :float))
(define-registered code_yankdup {} (yankduper :code))
(define-registered boolean_yankdup {} (yankduper :boolean))
(define-registered zip_yankdup {} (yankduper :zip))
(define-registered string_yankdup {} (yankduper :string))

(defn shover
  "Returns a function that shoves an item deep in the specified stack, using the top
   integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            item (top-item type with-index-popped)
            with-args-popped (pop-item type with-index-popped)
            actual-index (max 0 (min raw-index (count (type with-args-popped))))]
        (assoc with-args-popped type (let [stk (type with-args-popped)]
                                       (concat (take actual-index stk)
                                               (list item)
                                               (drop actual-index stk)))))
      state)))

(define-registered exec_shove {} (shover :exec))
(define-registered integer_shove {} (shover :integer))
(define-registered float_shove {} (shover :float))
(define-registered code_shove {} (shover :code))
(define-registered boolean_shove {} (shover :boolean))
(define-registered zip_shove {} (shover :zip))
(define-registered string_shove {} (shover :string))
