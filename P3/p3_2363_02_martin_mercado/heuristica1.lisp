;; Alias que aparece en el ranking 

(defvar *alias* '|PosicionSB|)

;; Función de evaluación heurística 

(defun eval-fn (player board)
    (let ((brd (get-board board)))
        (reduce #'+ 
            (mapcar 
                #'(lambda (row heu-row) 
                    (reduce #'+ 
                        (mapcar 
                            #'(lambda (colum heu-colum)
                                (cond 
                                    ((eq player colum) heu-colum)
                                    ((eq (opponent player) colum) (- 0 heu-colum))
                                    ((eq 0 colum) 0)))
                            row heu-row))) 
                brd heuristic))))

;; Funciones auxiliares

(defparameter heuristic '((50 -3 2 0 2 0 -3 50)
 (-3 -5 -1 -1 -1 -1 -5 -3)
 (2 -1 1 0 0 1 -1 2) 
 (2 -1 0 1 1 0 -1 2) 
 (2 -1 0 1 1 0 -1 2) 
 (2 -1 1 0 0 1 -1 2) 
 (-3 -5 -1 -1 -1 -1 -5 -3) 
 (50 -3 2 2 2 2 -3 50)))