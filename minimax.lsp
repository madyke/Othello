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
    ;(format t "Minimax Return: ~s~%" (minimax ply player player board -1000000 1000000))
    (caadr (minimax ply player player board -1000000 1000000))
)

(defun minimax (depth currPlayer maxPlayer board a b)

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
      (if (or (eq depth 0) (null (move-generator currPlayer board)))
        (list (static board maxPlayer ) nil)

        ; otherwise, generate successors and run minimax recursively
        (let*
            (
                (localBoard (copy-list board))

                ; generate list of sucessor positions
                (successors (move-generator currPlayer localBoard))

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score 
                    (if (string-equal currPlayer maxPlayer)
                        -2000000
                        2000000
                    )
                )

                ; other local variables
                succ-value
                succ-score
            )

            ; explore possible moves by looping through successor positions
            (dolist (successor successors)

                ;Reset localBoard for each successor
                (setf localBoard (flip-tiles successor currPlayer (copy-list board)))

                ; perform recursive DFS exploration of game tree
                (setq succ-value 
                    (minimax 
                        (1- depth)                 
			            (if (eq currPlayer 'black)
				            'white
				            'black
			            )
                        maxPlayer
                        localBoard
                        a
                        b
                    )
                )

                (setq succ-score (car succ-value))

                ;DEBUG
                ;( format t "~%~s: ~s: ~d~%" depth successor succ-score )

                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
                (cond 
                    ((string-equal currPlayer maxPlayer) 
                        (when 
                            (> succ-score best-score)
                            (setq best-score succ-score)
                            (setq best-path (cons successor (cdr succ-value)))
                        )
                        (setf a (max a best-score))
                    )
                    (t 
                        (when 
                            (< succ-score best-score)
                            (setq best-score succ-score)
                            (setq best-path (cons successor (cdr succ-value)))
                        )
                        (setf b (min b best-score))
                    )
                )
                  

                #|(when (<= b a)
                    ;(format t "PRUNE")
                    (return)
                )|#
            )

            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)


(defun static ( board player )
    ( let 
        ;Local var - hold results from corners heuristic
        ( ( results ( corners board player ) ) )

        ;Add scaled values for all heuristics
        ( +
            ;Coin parity heuristic
            ( * 10 ( coin board player ) )

            ;Corners heuristic
            ( * 801.724 ( car results ) )

            ;Corner neighbors heuristic
            ( * 382.026 ( cadr results ) )
        )
    )
)

(defun coin (board player)
    ( let 
        ;Local vars
        (
            ( maxPlayer ( if ( string-equal player 'black ) 'B 'W ) )
            ( minPlayer ( if ( string-equal player 'black ) 'W 'B ) )
        )
        
        ;Coin parity heuristic
        ( / ( - ( count-pieces maxPlayer board ) ( count-pieces minPlayer board ) )
            ( + ( count-pieces maxPlayer board ) ( count-pieces minPlayer board ) )
        )
    )
)

(defun corners (board player)
    ( let 
        ;Local vars
        (
            ;Max player color
            ( maxPlayer ( if ( string-equal player 'black ) 'B 'W ) )

            ;Min player color
            ( minPlayer ( if ( string-equal player 'black ) 'W 'B ) )

            ;Counters
            (maxCorners 0)
            (maxCloseCorners 0)
            (minCorners 0)
            (minCloseCorners 0)

            ;Positions of corners on board
            (corners '(0 7 56 63) )

            ;Placeholder for analyze results
            results
        )
        
        ;Loop over each corner
        (dolist (i corners)
            ;Analyze corner
            (setf results (analyzeCorner board maxPlayer minPlayer i) )

            ;Save results of analyze
            (setf maxCorners (+ (car results) maxCorners) )
            (setf maxCloseCorners (+ (cadr results) maxCloseCorners) ) 
            (setf minCorners (+ (caddr results) minCorners ) )
            (setf minCloseCorners (+ (cadddr results) minCloseCorners) )
        )

        ;Return scaled values for corners and close corners
        ( list
            ;Value for corners
            ( * 25 (- maxCorners minCorners ) )
            ;Value for close corners
            ( * -12.5 ( - maxCloseCorners minCloseCorners ) )
        )
    )    
)

(defun analyzeCorner (board maxPlayer minPlayer corner)
    (let 
        (
            ;List of positions neighboring corners
            (closeCorners 
                (cond
                    ((= corner 0) '(1 8 9))
                    ((= corner 7) '(6 14 15))
                    ((= corner 56) '(48 49 57))
                    ((= corner 63) '(54 55 62))
                )
            )

            ;Counters
            (maxCorners 0)
            (maxCloseCorners 0)
            (minCorners 0)
            (minCloseCorners 0)
        )

        ;Loop over all possibilities
        (cond
            ;If max player owns corner
            ((string-equal maxPlayer (nth corner board))
                (setf maxCorners (1+ maxCorners)))

            ;If min player owns corner
            ((string-equal minPlayer (nth corner board))
                (setf minCorners (1+ minCorners)))

            ;If noone owns corner, check corner neighbors
            (t
                ;Loop over corner neighbors
                (dolist ( i closeCorners )
                    ;Count who owns corner neighbors
                    (if (string-equal maxPlayer (nth i board))
                        (setf maxCloseCorners (1+ maxCloseCorners))
                        (setf minCloseCorners (1+ minCloseCorners))            
                    )
                )
            )
        )

        ;Return counters for corners and corner neighbors
        (list maxCorners maxCloseCorners minCorners minCloseCorners)     
    )
)

(defun mobility (board player)
    
)

(defun move-generator (player board)
    (get-moves player board)         
)
