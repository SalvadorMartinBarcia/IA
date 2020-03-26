(load "reversi-package")
(use-package 'reversi-package)

(reversi #'random-strategy #'random-strategy)

;; (reversi #'random-strategy #'human)

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

;; (reversi #'human (alpha-beta-searcher 2 #'count-difference))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

(round-robin
 (list (alpha-beta-searcher 2 #'count-difference)
       (alpha-beta-searcher 2 #'mobility)
       (alpha-beta-searcher 2 #'eval-fn))
 5
 10
 '(count-difference
   mobility
   eval-fn)
 )

