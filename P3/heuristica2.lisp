;; Alias que aparece en el ranking 
;; posible estrategia combinar movimientos del enemigo no sumandole y aumentando nuestras fichas

(defvar *alias* '|Movility|) 

;; Función de evaluación heurística 

(defun eval-fn (player board)
    "The number of moves a player has."
    (length (legal-moves player board)))

;; Funciones auxiliares
