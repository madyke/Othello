#|
 | Program: Othello
 | File: minimax.lsp
 | Authors: Matt Dyke, Christian Sieh
 |
 | Description: This file contains the minimax function, along with the 
 |   heuristics that are used in the static evaluation function.
 |#

;--------------------------------- Functions ----------------------------------;


#|
 | Function: make-move
 |
 | Description: This function calls the minimax routine with the necessary
 |   parameters. It is primarily a wrapper function for use in tournament play.
 |
 | Parameters:
 |   board - Current board state
 |   player - Current player's color
 |   ply - How far to search for minimax
 |
 | Return:
 |   ( row col ) - Best position available from minimax
 |#
(defun make-move (board player ply)
    (when (eq player 'B)
        (setf player 'black)
    )
    (when (eq player 'W)
        (setf player 'white)
    )
    
    ;Perform minimax and return the first move in best-path list
    (caadr (minimax ply player player board -1000000 1000000))
)

 
#|
 | Function: minimax
 |
 | Description: Generalized recursive minimax routine. This function was heavily
 |   adapted from code provided by Dr. Weiss.
 |
 | Parameters:
 |   depth - How many levels to search in with minimax
 |   currPlayer - Current player's color
 |   maxPlayer - Which player is trying to maximize their result
 |   board - Current board state
 |   a - Alpha bound for a-b pruning
 |   b - Beta bound for a-b pruning
 |
 | Return:
 |   ( best-score best-path ) - Best score from minimax and path to that node
 |#
(defun minimax (depth currPlayer maxPlayer board a b)

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
      (if (or (eq depth 0) (null (get-moves currPlayer board)))
        (list (static board maxPlayer ) nil)

        ; otherwise, generate successors and run minimax recursively
        (let*
            (
                (localBoard (copy-list board))

                ; generate list of sucessor positions
                (successors (get-moves currPlayer localBoard))

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

                ;Extract score from minimax return
                (setq succ-score (car succ-value))

                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
                (cond 
                    ;If the current player is trying to maximize the score
                    ((string-equal currPlayer maxPlayer) 
                        ;If current result is better than best so far, save it
                        (when (> succ-score best-score)
                            (setq best-score succ-score)
                            (setq best-path (cons successor (cdr succ-value)))
                        )
                        
                        ;If best result is greater than current alpha, save it
                        (setf a (max a best-score))
                    )
                    ;If the current player is trying to minimize the score
                    (t 
                        ;If current result is better than best so far, save it
                        (when (< succ-score best-score)
                            (setq best-score succ-score)
                            (setq best-path (cons successor (cdr succ-value)))
                        )
                        
                        ;If best result is smaller than current beta, save it
                        (setf b (min b best-score))
                    )
                )
            )

            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)

 
#|
 | Function: static
 |
 | Description: This is our static evaluation function. It was adopted from
 |   https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/,
 |   which was written by Kartik Kukreja. We used only a few of the heuristics
 |   he mentions. We implemented a heuristic based on coin parity, corner
 |   ownership, corner neighbor ownership, and mobility. Each heuristic is 
 |   scaled by a certain factor which was determined by Kartik Kukreja. Because
 |   the mobility heuristic greatly increased the AI's playing time, we do not
 |   actually use it our code, though we have left our implementation in as a
 |   guide for how it could work.
 |
 | Parameters:
 |   board - Current board state
 |   maxPlayer - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for static evaluation function for current board
 |#
(defun static ( board maxPlayer )
    ( let 
        ;Local var - hold results from corners heuristic
        ( ( results ( cornersHeuristic board maxPlayer ) ) )

        ;Add scaled values for all heuristics
        ( +
            ;Coin parity heuristic
            ( * 10 ( coinHeuristic board maxPlayer ) )

            ;Corners heuristic
            ( * 801.724 ( car results ) )

            ;Corner neighbors heuristic
            ( * 382.026 ( cadr results ) )

            ;Mobility heuristic
            ;( * 78.922 ( mobilityHeuristic board maxPlayer ) )
        )
    )
)

 
#|
 | Function: coinHeuristic
 |
 | Description: This the coin parity heuristic, which was adopted from the 
 |   website mentioned in the static function documentation.
 |
 | Parameters:
 |   board - Current board state
 |   maxPlayer - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for coin heuristic for current board
 |#
(defun coinHeuristic (board maxPlayer)
    ( let 
        ;Local vars
        (
            ( maxPlayer ( if ( string-equal maxPlayer 'black ) 'B 'W ) )
            ( minPlayer ( if ( string-equal maxPlayer 'black ) 'W 'B ) )
        )
        
        ;Coin parity heuristic
        ( * 100 ( / ( - ( count-pieces maxPlayer board ) ( count-pieces minPlayer board ) )
                    ( + ( count-pieces maxPlayer board ) ( count-pieces minPlayer board ) )
        ))
    )
)

 
#|
 | Function: cornersHeuristic
 |
 | Description: This the corners and courner neighbors ownership heuristic, 
 |   which was adopted from the website mentioned in the static function 
 |   documentation.
 |
 | Parameters:
 |   board - Current board state
 |   maxPlayer - Which player is trying to maximize their result
 |
 | Return:
 |   (corners closeCorners) - Scores for corners heuristic and corner neighbors
 |                            heuristic
 |#
(defun cornersHeuristic (board maxPlayer)
    ( let 
        ;Local vars
        (
            ;Max player color
            ( maxPlayer ( if ( string-equal maxPlayer 'black ) 'B 'W ) )

            ;Min player color
            ( minPlayer ( if ( string-equal maxPlayer 'black ) 'W 'B ) )

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

 
#|
 | Function: analyzeCorner
 |
 | Description: This is a helper function for cornersHeuristic. It examines the 
 |   given corner position for how many pieces are owned by the min and max 
 |   players for the corner and its neighbors.
 |
 | Parameters:
 |   board - Current board state
 |   maxPlayer - Which player is trying to maximize their result
 |   minPlayer - Which player is trying to minimize their result
 |   corner - What is the position of the corner being examined
 |
 | Return:
 |   (maxCorners maxCloseCorners minCorners minCloseCorners)
 |      - How many corners and corner neighbors are owned by the max player and
 |        by the min player
 |#
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

 
#|
 | Function: mobilityHeuristic
 |
 | Description: This the mobility heuristic, which was adopted from the website
 |   mentioned in the static function documentation. It attempts to compare how
 |   many moves each player will after for the current board position.
 |
 | Parameters:
 |   board - Current board state
 |   maxPlayer - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for coin heuristic for current board
 |#
(defun mobilityHeuristic (board maxPlayer)
    (let 
        ( 
            (minPlayer (if (string-equal maxPlayer 'black) 'white 'black))
            maxMoves
            minMoves 
        )

        ;Get number of moves for each player
        (setf maxMoves ( length (get-moves maxPlayer board)))
        (setf minMoves ( length (get-moves minPlayer board)))

        ;Calculate heuristic value
        ( if ( > maxMoves minMoves )
            ;If max has more moves
            ( / ( * 100 maxMoves ) ( + maxMoves minMoves ) )

            ;If min has more moves
            ( / ( * -100 minMoves ) ( + maxMoves minMoves ) )
        )
    )
)