;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
    "Zero of a function using the Newton-Raphson method


      INPUT:  f:        function whose zero we wish to find
              df-dx:    derivative of f
              max-iter: maximum number of iterations
              x0:       initial estimation of the zero (seed)
              tol-abs:  tolerance for convergence


      OUTPUT: estimation of the zero of f, NIL if not converged"


              (let ((xN (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
                (cond ((<= max-iter 0)
                        nil)
                    ((<
                      (abs (- 0 (/ (funcall f x0) (funcall df-dx x0))))
                      tol-abs)
                        xN)
                    ((>=
                      (abs (- 0 (/ (funcall f x0) (funcall df-dx x0))))
                      tol-abs)
                        (newton f df-dx (- max-iter 1) xN tol-abs)))))
;;; Ejemplos:
;;;   (newton #'sin #'cos 50 2.0) ;-> 3.1415921 ;Caso del enunciado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
 "Zeros of a function using the Newton-Raphson method


   INPUT:  f:        function whose zero we wish to find
           df-dx:    derivative of f
           max-iter: maximum number of iterations
           seeds:    list of initial estimations of the zeros
           tol-abs:  tolerance for convergence


   OUTPUT: list of estimations of the zeros of f"
          (cond
            ((null seeds) nil))
            (mapcar #'(lambda (x0) (newton f df-dx max-iter x0 tol-abs)) seeds))

;;; Ejemplos:
;;;   (newton-all #'sin #'cos 50 (mapcar #'eval '((/ pi 2) 1.0 2.0 4.0 6.0))) ;->(NIL 0.0 3.1415927 3.1415927 6.2831855) ;Caso del enunciado
;;;   (newton-all #'sin #'cos 50 (mapcar #'eval '())) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst)
  "Combines an element with all the elements of a list


    INPUT:  elt: element
            lst: list


    OUTPUT: list of pairs, such that
               the first element of the pair is elt.
               the second element is an element from lst"

	 (if	(null lst)
	  nil
	  (append (list (list elt (first lst))) (combine-elt-lst elt (rest lst)))))

;;; Ejemplos:
;;;   (combine-elt-lst 'a '(1 2 3)) ;->((A 1) (A 2) (A 3)) ;Caso del enunciado
;;;   (combine-elt-lst 'a '()) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
  "Calculates the cartesian product of two vectors


    INPUT:  lst1: lst
            lst2: lst


    OUTPUT: list of pairs, such that
              the first element is an element from lst1.
              the second element is an element from lst2"

     (if	(null lst1)
    	nil
      (append
        (combine-elt-lst(first lst1) lst2)
        (combine-lst-lst(rest lst1) lst2))))

;;; Ejemplos:
;;;   (combine-lst-lst '(a b c) '(1 2)) ;->((A 1) (A 2) (B 1) (B 2) (C 1) (C 2)) ;Caso del enunciado
;;;   (combine-lst-lst '(a b c) '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (combine-lst-lst '() '(1 2)) ;-> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun combine-list-of-lsts-aux (lst1 all)
      (if (null all)
        lst1
        (combine-list-of-lsts-aux
          (mapcan
            #'(lambda (x1)
              (mapcar
                 #'(lambda (x2)
                  (if (listp x1)
                    (append x1 (list x2))
                    (list x1 x2)))
                (first all)))
          lst1)
          (rest all)))
  )

  (defun combine-list-of-lsts (lstolsts)
    "Combinations of N elements, each of wich


     INPUT:  lstolsts: list of N sublists (list1 ... listN)


     OUTPUT: list of sublists of N elements, such that in each
             sublist the first element is from list1
                   the second element is from list 2
                   ...
                   the Nth element is from list N"
          (if (null lstolsts)
            nil
          (combine-list-of-lsts-aux (first lstolsts) (rest lstolsts))
          ))
;;; Ejemplos:
;;;   (combine-lst-lst '(a b c) '(1 2)) ;->((A 1) (A 2) (A 3)) ;Caso del enunciado
;;;   (combine-lst-lst '(a b c) '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (combine-lst-lst '() '(1 2)) ;-> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scalar-product (x y)
  "Calculates the scalar product of two vectors

   INPUT:  x: vector, represented as a list
           y: vector, represented as a list

   OUTPUT: scalar product between x and y


   NOTES:
        * Implemented with mapcar"
        (cond
          ((null x) nil)
          ((null y) nil)
          (T (apply #'+ (mapcar #'* x y)))))
;;; Ejemplos:
;;;   (scalar-product '(1 2 3) '(3 -1 5)) ;->16 ;Caso del enunciado
;;;   (scalar-product '() '(3 -1 5)) ; -> NIL ;Caso de parámetros erroneos
;;;   (scalar-product '(1 2 3) '()) ;-> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm


(defun euclidean-norm (x)
  "Calculates the euclidean (l2) norm of a vector

    INPUT:  x: vector, represented as a list


    OUTPUT: euclidean norm of x"
    (if (null x)
      nil
      (sqrt (scalar-product x x))))

;;; Ejemplos:
;;;   (euclidean-norm '(3 -1 5)) ;->5.9161 ;Caso del enunciado
;;;   (euclidean-norm '()) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance


(defun euclidean-distance (x y)
  "Calculates the euclidean (l2) distance between two vectors

    INPUT: x: vector, represented as a list
           y: vector, represented as a list


    OUTPUT: euclidean distance between x and y"
    (cond
      ((null x) nil)
      ((null y) nil)
      (T (euclidean-norm (mapcar #'- x y)))))

;;; Ejemplos:
;;;   (euclidean-distance '(1 2 3) '(3 -1 5)) ;->4.1231 ;Caso del enunciado
;;;   (euclidean-distance '(1 2 3) '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (euclidean-distance '() '(3 -1 5)) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-similarity (x y)
  "Calculates the cosine similarity between two vectors


    INPUT:  x: vector, representad as a list
            y: vector, representad as a list


    OUTPUT: cosine similarity between x and y


    NOTES:
       * Evaluates to NIL (not defined)
         if at least one of the vectors has zero norm.
       * The two vectors are assumed to have the same length

       -en = euclidean-norm
       -sp = scalar-product"

       (cond
         ((null x) nil)
         ((null y) nil)
         (t (let ((en (* (euclidean-norm x) (euclidean-norm y)))
            (sp (scalar-product x y)))
              (if (zerop en)
                nil
                (/ sp en))))))
;;; Ejemplos:
;;;   (cosine-similarity '(1 2 3) '(-2 1 3)) ;->0.6429 ;Caso del enunciado
;;;   (cosine-similarity '(1 2 3) '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (cosine-similarity '() '(-2 1 3)) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angular-distance (x y)
  "Calculates the angular distance between two vectors


   INPUT:  x: vector, representad as a list
           y: vector, representad as a list


   OUTPUT: cosine similarity between x and y


   NOTES:
      * Evaluates to NIL (not well defined)
        if at least one of the vectors has zero norm.
      * The two vectors are assumed to have the same length"

      (cond
        ((null x) nil)
        ((null y) nil)
        (t (/ (acos (cosine-similarity x y)) pi))))
;;; Ejemplos:
;;;   (angular-distance '(1 2 3) '(-2 1 3)) ;->0.2777 ;Caso del enunciado
;;;   (angular-distance '(1 2 3) '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (angular-distance '() '(-2 1 3)) ; -> NIL ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors


(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    "Selects from a list the vectors whose similarity to a
     test vector is above a specified threshold.
     The resulting list is ordered according to this similarity.

     INPUT:  lst-vectors:   list of vectors
             test-vector:   test vector, representad as a list
             similarity-fn: reference to a similarity function
             threshold:     similarity threshold (default 0)

     OUTPUT: list of pairs. Each pair is a list with
             a vector and a similarity score.
             The vectors are such that their similarity to the
             test vector is above the specified threshold.
             The list is ordered from larger to smaller
             values of the similarity score

     NOTES:
        * Uses remove-if and sort"

        (cond
          ((null lst-vectors) nil)
          ((null test-vector) nil)
          (t (sort
          (remove-if
            #'(lambda (x1) (< (second x1) threshold))
             (mapcar #'(lambda (x0)
                (list x0 (funcall similarity-fn test-vector x0)))
              lst-vectors))
            #'> :key #'second))))
;;; Ejemplos:
;;;   (select-vectors '((-1 -1 -1) (-1 -1 1) (-1 1 1) (1 1 1))
;;;   '(1 1 1) #'cosine-similarity 0.2) ;->(((1 1 1) 1.0) ((-1 1 1) 0.33333334)) ;Caso del enunciado
;;;   (select-vectors '()
;;;   '(1 1 1) #'cosine-similarity 0.2) ;->nil ;Caso de parámetros erroneos
;;;   (select-vectors '((-1 -1 -1) (-1 -1 1) (-1 1 1) (1 1 1))
;;;   '() #'cosine-similarity 0.2) ;->nil ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defun nearest-neighbor (lst-vectors test-vector distance-fn)
  "Selects from a list the vector that is closest to the
   reference vector according to the specified distance function

   INPUT:  lst-vectors:   list of vectors
           ref-vector:    reference vector, represented as a list
           distance-fn:   reference to a distance function

   OUTPUT: List formed by two elements:
           (1) the vector that is closest to the reference vector
               according to the specified distance function
           (2) The corresponding distance value.


   NOTES:
      * The implementation is recursive
      * It ignores the vectors in lst-vectors for which the
        distance value cannot be computed."

        (cond
          ((null lst-vectors) nil)
          ((null test-vector) nil)
          (t (let
            ((x0
              (list (first lst-vectors)
                (funcall distance-fn test-vector (first lst-vectors)))))
            (if (null (rest lst-vectors))
              x0
              (first
                (sort
                  (list
                    x0
                    (nearest-neighbor
                      (rest lst-vectors)
                      test-vector
                      distance-fn))
                #'< :key #'second)))))))
;;; Ejemplos:
;;;   (nearest-neighbor '((-1 -1 -1) (-2 2 2) (-1 -1 1))
;;;   '(1 1 1) #'angular-distance)  ;-> ((-2 2 2) 0.3918)  ;Caso del enunciado
;;;   (nearest-neighbor '((-1 -1 -1) (-2 2 2) (-1 -1 1))
;;;   '() #'angular-distance)  ;->nil ;Caso de parámetros erroneos
;;;   (nearest-neighbor '()
;;;   '(1 1 1) #'angular-distance)  ;->nil ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (defun backward-chaining-aux(goal lst-rules pending-goals)

      (some
        #'(lambda (x0) (not (null x0)))
        (mapcar #'(lambda (rule)
          (when (eq goal (second rule))
            (every
              #'(lambda (x1) (not (null x1)))
              (mapcar #'(lambda (go)
                (backward-chaining-aux
                  go
                  (remove rule lst-rules :test #'equal)
                    (if (null pending-goals)
                      (list goal)
                      (cons goal pending-goals))))
                (first rule)))))
          lst-rules)))

  (defun backward-chaining (goal lst-rules)
    "Backward-chaining algorithm for propositional logic

     INPUT: goal:      symbol that represents the goal
            lst-rules: list of pairs of the form
                       (<antecedent>  <consequent>)
                       where <antecedent> is a list of symbols
                       and  <consequent> is a symbol


     OUTPUT: T (goal derived) or NIL (goal cannot be derived)


     NOTES:
          * Implemented with some, every"


          (if (null lst-rules)
            nil
            (backward-chaining-aux goal lst-rules NIL)))

;;; Ejemplos:
;;;   siendo goal 'q y lst-rules '((NIL A) (NIL B) ((P) Q) ((L M) P) ((B L) M) ((A P) L) ((A B) L)))
;;;   (backward-chaining goal lst-rules) ;->T ;Caso del enunciado
;;;   (backward-chaining goal '()) ; -> NIL ;Caso de parámetros erroneos
;;;   (backward-chaining 't lst-rules) ; -> NIL ;Otro caso en el que no se satisface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun new-paths (path node net)
 (mapcar #'(lambda(n)
       (cons n path))
               (rest (assoc node net))))

(defun bfs (end queue net)
  (if (null queue)
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
          (bfs end
               (append (rest queue)
                       (new-paths path node net))
               net)))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;; Ejemplos:
;;;   (shortest-path 'c 'd '((a d) (b d f) (c e) (d f) (e b f) (f))) ;->(C E B D) ;Caso del enunciado
;;;   (shortest-path 'c 'f '((a b c d e) (b a d e f) (c a g) (d a b g h)
;;;   (e a b g h) (f b h) (g c d e h) (h d e f g))) ; -> (C A B F) ;Caso 2 del enunciado
;;;   (shortest-path 'c 'f '((a b) (b c) (c a) (d a))) ; -> entra en un bucle infinito ;Caso de parámetros erroneos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfs-improved (end queue net)
  (if (null queue)
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
          (bfs-improved end
               (append (rest queue)
                       (new-paths path node net))
               (remove (assoc node net) net  :test #'equal))))))




(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net))

;;; Ejemplos:
;;;   (shortest-path-improved 'c 'd '((a d) (b d f) (c e) (d f) (e b f) (f))) ;->(C E B D) ;Caso del enunciado
;;;   (shortest-path-improved 'c 'f '((a b c d e) (b a d e f) (c a g) (d a b g h)
;;;   (e a b g h) (f b h) (g c d e h) (h d e f g))) ; -> (C A B F) ;Caso 2 del enunciado
;;;   (shortest-path-improved 'c 'f '((a b) (b c) (c a) (d a))) ; -> nil ;Caso en el que el otro algoritmo fallaba
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
