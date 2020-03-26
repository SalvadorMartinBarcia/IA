;; Alias que aparece en el ranking 

(defvar *alias* '|count-difference|)

;; Función de evaluación heurística 

(defun eval-fn (player board)
    "Count player's pieces minus opponent's pieces."
    (let ((brd (get-board board)))
        (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
        (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

;; Funciones auxiliares
