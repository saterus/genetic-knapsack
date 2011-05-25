
;; Generic Genetic Algorithm Solving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip (a b)
  (cond ((or (null a) (null b)) '())
        (t (cons (list (car a) (car b)) (zip (cdr a) (cdr b))))))

(defun compare-fst (a b)
  (< (car a) (car b)))

(defun solve (pop fit)
  (let ((ranked (sort (zip (map #'fit pop) pop) :key compare-fst ))
        )))

(defun new-child (sorted)
  (let* ((a (select-parent sorted))
         (b (select-parent (remove a sorted :test #'(lambda (x y) (eql (cadr y) x))))))
    ;; (reproduce a b)))
    (format t "Parent A: ~A      Parent B: ~A~%" a b)))

(defun select-parent (sorted)
  (let* ((x (random (length sorted)))
         (y (if (<= (1- x) 0) 0 (random (1- x)))))
    (format t "x: ~A, y: ~A, z: ~A, e: ~A~%" x y (- x y) (elt sorted (- x y)))
    (cadr (elt sorted (- x y)))))


;; todo: mutate
(defun reproduce (x y)
  (let ((crossover (random (length x)))
        (offspring (make-array (length x) :initial-contents x)))
    (dotimes (i (- (length y) crossover) offspring)
         (setf (aref offspring i) (aref y i)))))



;; Knapsack Specific Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Struct representing an item to be placed in the knapsack
;; defines make-item, item-p, copy-item, item-size, item-value
(defstruct item size value)

;; a Sack is a bit array of items in the sack
(defun gen-random-sack (items)
  )

(defun read-items (path) )

;; list of sacks
(defun ks-population (size)
  ;; (read-items)
  )

(defparameter ks-size 500)
(defparameter overstuff-penalty-constant 10)

;; f(s) = c(ks-size - sack-size) + sack-value
(defun ks-fitness (sack)
  (+ (* overstuff-penalty-constant
        (- ks-size
           (sack-size sack)))
     (sack-value sack)))

;; Solution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (solve ks-population, ks-fitness)
