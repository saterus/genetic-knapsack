
;; Generic Genetic Algorithm Solving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve (pop, fit) )

(defun fitness () )

(defun select-parents (x,y) )

(defun reproduce () )




;; Knapsack Specific Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Struct representing an item to be placed in the knapsack
;; defines make-item, item-p, copy-item, item-size, item-value
(defstruct item size value)

(defun read-items (path) )

(defun ks-population ()
  ;; (read-items)
  )

(defun ks-fitness () )

;; Solution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (solve ks-population, ks-fitness)
