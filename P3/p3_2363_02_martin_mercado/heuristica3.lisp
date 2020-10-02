;; Alias que aparece en el ranking 

(defvar *alias* '|Artorias|)

;; Función de evaluación heurística 

(defun eval-fn (player board)
    (let (
        (cp (coin-parity player board))
        (mo (mobilitySB player board))
        (po (pos player board))
        (tiempo (fichas-tablero player board)))
        
        (cond 
            ((< tiempo 50) (+ cp mo po))
            ((< tiempo 60) (+ cp (* 2 mo) po))
            ((< tiempo 64) (+ (* 2 cp) mo po))
            ((eq tiempo 64) cp))))

;; Funciones auxiliares

(defun fichas-tablero (player board)
    (- 64 (reduce #'+ (mapcar #'(lambda (row) (count 0 row)) (get-board board)))))

(defun coin-parity (player board)
    (let (
        (play (reduce #'+ (mapcar #'(lambda (row) (count player row)) (get-board board))))
        (opp (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) (get-board board)))))

        (if (eq 0 (+ play opp))
            0
            (* 100 (/ (- play opp) (+ play opp))))))

(defun mobilitySB (player board)
    (let (
        (play (length (legal-moves player board)))
        (opp (length (legal-moves (opponent player) board))))
        
        (if (eq 0 (+ play opp))
            0
            (* 100 (/ (- play opp) (+ play opp))))))

(defun pos (player board)
    (let (
        (resta (position-aux player board heuristic-2))
        (total 300))
    (* 420 (/ resta total))))

(defun position-aux (player board heur)
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
                brd heur))))

(defparameter heuristic-2 '((50 -10 2 2 2 2 -10 50)
 (-10 -25 -1 -1 -1 -1 -25 -10)
 (2 -1 1 0 0 1 -1 2) 
 (2 -1 0 1 1 0 -1 2) 
 (2 -1 0 1 1 0 -1 2) 
 (2 -1 1 0 0 1 -1 2) 
 (-10 -25 -1 -1 -1 -1 -25 -10) 
 (50 -10 2 2 2 2 -10 50)))
