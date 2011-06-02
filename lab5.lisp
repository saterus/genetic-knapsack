;; Generic Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip (a b)
  (cond ((or (null a) (null b)) '())
        (t (cons (list (car a) (car b))
                 (zip (cdr a) (cdr b))))))

(defun sum (num-list &key key)
  (reduce #'+ num-list :key key))

(defun gt-fst (a b)
  (> (car a) (car b)))

(defun random-bool ()
  (if (= 1 (random 2))
      T
      NIL))

;; Generic Genetic Algorithm Solving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *num-generations* 36)
(defparameter *mutation-rate* 10)

;; Solve the problem using the Genetic Algorithm
(defun solve (pop fit)
  (solve-generation pop fit *num-generations* 0))

;; Run the algorithm a maximum number of generations, then return the
;; current population. Alternatively, we could run until a convergence.
(defun solve-generation (pop fit max-generation generation-count)
    (cond ((= max-generation generation-count) pop)
          (t (let* ((paired (zip (mapcar fit pop) pop))
                    (ranked (sort paired #'> :key #'car ))
                    (new-pop (gen-pop ranked '())))
               (solve-generation new-pop fit max-generation (1+ generation-count))))))

;; Generate the new population out of the new children.
(defun gen-pop (sorted new-pop)
  (cond ((= (length sorted) (length new-pop))
         new-pop)
        (t (gen-pop sorted (cons (new-child sorted) new-pop)))))

;; Pick two parents, and recombine their DNA to produce a new child.
(defun new-child (sorted)
  (let* ((a (select-parent sorted))
         (b (select-parent (remove a sorted :test #'(lambda (x y) (eql (cadr y) x))))))
    ;; (format t "Parent A: ~A      Parent B: ~A~%" a b)
    (reproduce a b)))

;; Parent A is a random parent.
;; Parent B is a "Better" parent than Parent A.
;; The better the parent is, the more offspring it produces.
(defun select-parent (sorted)
  (let* ((x (random (length sorted)))
         (y (if (<= (1- x) 0) 0 (random (1- x)))))
    ;; (format t "x: ~A, y: ~A, z: ~A, e: ~A~%" x y (- x y) (elt sorted (- x y)))
    (cadr (elt sorted (- x y)))))

;; Pick a random crossover point for the DNA, then mutate.
(defun reproduce (x y)
  (let ((crossover (random (length x)))
        (offspring (make-array (length x) :initial-contents x)))
    (dotimes (i (- (length y) crossover))
      (setf (aref offspring i) (aref y i)))
    (mutate offspring)))

;; Logical-not a gene with some probability.
(defun mutate (state)
  (if (< (random 100) *mutation-rate*)
      (let ((index (random (length state))))
        (setf (aref state index) (not (aref state index)))
        state)
      state))

;; Creates random DNA sequences.
(defun gen-random-dna (arr-size)
  (let ((arr (make-array arr-size)))
    (dotimes (i arr-size arr)
      (setf (aref arr i) (random-bool)))))


;; Knapsack Specific Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ks-items* NIL)
(defparameter *ks-pop* NIL)
(defparameter *ks-size* 500)
(defparameter *ks-overstuff-penalty* 20)
(defparameter *ks-pop-size* 200)

;; Struct representing an item to be placed in the knapsack
;; defines make-item, item-p, copy-item, item-size, item-value
(defstruct item name size value)

;; Reads the possible items from a file.
(defun read-items (path)
  (with-open-file (str path :direction :input :if-does-not-exist :error)
    (let ((items '()))
      (do ((line (read-line str nil 'eof)
                 (read-line str nil 'eof)))
          ((eql line 'eof))
        (push (read-from-string line) items))
      (reverse items))))

;; a Sack is a bool array indicating which of the items are in the sack
(defun fill-items (path)
  (let* ((item-list (read-items path))
         (item-array (make-array (length item-list))))
    (map-into item-array #'identity item-list)))

(defun ks-population (size)
  (cond ((null *ks-pop*)
         (setf *ks-pop* (ks-generate-population size)))
        (t *ks-pop*)))

(defun ks-generate-population (size)
  (let ((pop '()))
    (dotimes (i size pop)
      (push (gen-random-dna (length *ks-items*)) pop))))

;; Translates a DNA-array into a list of items in the sack.
;; Useful for computing fitness.
(defun items-in-sack (bool-arr)
  (let ((item-list '()))
    (dotimes (i (length bool-arr) (reverse item-list))
      (if (aref bool-arr i)
          (push (aref *ks-items* i) item-list)))))

(defun sack-size (sack)
  (sum sack :key #'item-size))

(defun sack-value (sack)
  (sum sack :key #'item-value))

;; f(s) = sack-value                                     | if (*ks-size* - sack-size) >= 0
;; f(s) = over_const(*ks-size* - sack-size) + sack-value | otherwise
(defun ks-fitness (state)
  (let ((sack (items-in-sack state)))
    (cond ((> (sack-size sack) *ks-size*)
           (+ (* *ks-overstuff-penalty*
                 (- *ks-size* (sack-size sack)))
              (sack-value sack)))
          (t (sack-value sack)))))


;; Solution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-solutions (solutions)
  (mapcar #'(lambda (soln)
              (let ((soln-sack (items-in-sack (cadr soln))))
                (format t "Score: ~S  Size: ~S  Value: ~S   Items: ~A~%"
                        (car soln)
                        (sack-size soln-sack)
                        (sack-value soln-sack)
                        (mapcar #'item-name soln-sack))))
          (sort solutions #'> :key #'car)))

;; todo: pull this from command line args
(setf *ks-items* (fill-items (make-pathname :name "camping_items.lisp")))

(defun solve-new (size)
  (setf *ks-pop* nil)
  (ks-population size)
  (let ((solns (solve *ks-pop* #'ks-fitness)))
    (zip (mapcar #'ks-fitness solns) solns)))

(defun main ()
  (print-solutions (solve-new *ks-pop-size*))
  (quit))
