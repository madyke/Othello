#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

#|
 | 	This is the move function used for the AI in the tournament
 |#
(defun make-move (board player ply)
    (when (eq player 'B)
        (setf player 'black)
    )
    (when (eq player 'W)
        (setf player 'white)
    )
    ;(format t "Minimax Return: ~s~%" (caadr (minimax 0 ply player board)))
    (caadr (minimax ply player board))
)

(defun minimax (depth player board)

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
      (if (or (eq depth 0) (null (move-generator player board)))
        (list (static board player ) nil)

        ; otherwise, generate successors and run minimax recursively
        (let*
            (
                (localBoard (copy-list board))

                ; generate list of sucessor positions
                (successors (move-generator player localBoard))

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -1000000)

                ; other local variables
                succ-value
                succ-score
            )

            ; explore possible moves by looping through successor positions
            (dolist (successor successors)

                ;Reset localBoard for each successor
                (setf localBoard (flip-tiles successor player (copy-list board)))

                ; perform recursive DFS exploration of game tree
                (setq succ-value 
                    (minimax 
                        (1- depth)                 
			            (if (eq player 'black)
				            'white
				            'black
			            )
                        localBoard
                    )
                )

                ; change sign every ply to reflect alternating selection
                ; of MAX/MIN player (maximum/minimum value)
                (setq succ-score (- (car succ-value)))
                
                ;DEBUG
                ( format t "~%~s: ~s: ~d~%" depth successor succ-value )

                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
                (when (> succ-score best-score)
                      (setq best-score succ-score)
                      (setq best-path (cons successor (cdr succ-value)))
                )

            )

            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)


(defun static ( board player )
    ( let 
        ;Local vars
        (
            ( currPlayer ( if ( string-equal player 'black ) 'B 'W ) )
            ( opponent ( if ( string-equal player 'black ) 'W 'B ) )
        )
        
        ;Coin parity heuristic
        ( / ( - ( count-pieces currPlayer board ) ( count-pieces opponent board ) )
            ( + ( count-pieces currPlayer board ) ( count-pieces opponent board ) )
        )
    )
)

(defun move-generator (player board)
    (get-moves player board)         
)
