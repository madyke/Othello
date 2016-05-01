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
( defun make-move ( board player ply )
    ( when ( eq player 'B )
        ( setf player 'black )
    )
    ( when ( eq player 'W )
        ( setf player 'white )
    )
    
    ;Perform minimax and return the first move in best-path list
    ( caadr ( minimax ply player player board -1000000 1000000 ) )
)

 
#|
 | Function: minimax
 |
 | Description: Generalized recursive minimax routine. This function was heavily
 |   adapted from code provided by Dr. Weiss.
 |
 | Parameters:
 |   depth - How many levels to search in with minimax
 |   curr-player - Current player's color
 |   max-player - Which player is trying to maximize their result
 |   board - Current board state
 |   a - Alpha bound for a-b pruning
 |   b - Beta bound for a-b pruning
 |
 | Return:
 |   ( best-score best-path ) - Best score from minimax and path to that node
 |#
(defun minimax ( depth curr-player max-player board a b )

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    ( if ( or 
                ( eq depth 0 ) 
                ( null ( get-moves curr-player board ) ) 
         )
        ( list ( static board max-player ) nil )

        ; otherwise, generate successors and run minimax recursively
        ( let*
            (
                ( localBoard ( copy-list board ) )

                ; generate list of sucessor positions
                ( successors ( get-moves curr-player localBoard ) )

                ; initialize current best path to nil
                ( best-path nil )

                ; initialize current best score to negative infinity
                ( best-score 
                    ( if ( string-equal curr-player max-player )
                        -2000000
                        2000000
                    )
                )

                ; other local variables
                succ-value
                succ-score
            )

            ; explore possible moves by looping through successor positions
            ( dolist ( successor successors )

                ;Reset localBoard for each successor
                ( setf localBoard ( flip-tiles successor curr-player ( copy-list board ) ) )

                ; perform recursive DFS exploration of game tree
                ( setq succ-value 
                    ( minimax 
                        ( 1- depth )                 
			            ( if ( eq curr-player 'black )
				            'white
				            'black
			            )
                        max-player
                        localBoard
                        a
                        b
                    )
                )

                ;Extract score from minimax return
                ( setq succ-score ( car succ-value ) )

                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
                ( cond 
                    ;If the current player is trying to maximize the score
                    ( ( string-equal curr-player max-player ) 
                        ;If current result is better than best so far, save it
                        ( when ( > succ-score best-score )
                            ( setq best-score succ-score )
                            ( setq best-path ( cons successor ( cdr succ-value ) ) )
                        )
                        
                        ;If best result is greater than current alpha, save it
                        ( setf a ( max a best-score ) )
                    )
                    ;If the current player is trying to minimize the score
                    ( t 
                        ;If current result is better than best so far, save it
                        ( when ( < succ-score best-score )
                            ( setq best-score succ-score )
                            ( setq best-path ( cons successor ( cdr succ-value ) ) )
                        )
                        
                        ;If best result is smaller than current beta, save it
                        ( setf b ( min b best-score ) )
                    )
                )
                
                ;Prune for alpha-beta cutoffs
                (when ( <= b a ) ( return ) )
            )

            ; return (value path) list when done
            ( list best-score best-path )
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
 |   max-player - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for static evaluation function for current board
 |#
(defun static ( board max-player )
    ( let 
        ;Local var - hold results from corners heuristic
        ( ( results ( corner-heuristic board max-player ) ) )

        ;Add scaled values for all heuristics
        ( +
            ;Coin parity heuristic
            ( * 10 ( coin-heuristic board max-player ) )

            ;Corners heuristic
            ( * 801.724 ( car results ) )

            ;Corner neighbors heuristic
            ( * 382.026 ( cadr results ) )

            ;Mobility heuristic
            ;( * 78.922 ( mobility-heuristic board max-player ) )
        )
    )
)

 
#|
 | Function: coin-heuristic
 |
 | Description: This the coin parity heuristic, which was adopted from the 
 |   website mentioned in the static function documentation.
 |
 | Parameters:
 |   board - Current board state
 |   max-player - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for coin heuristic for current board
 |#
(defun coin-heuristic (board max-player)
    ( let 
        ;Local vars
        (
            ( max-player ( if ( string-equal max-player 'black ) 'B 'W ) )
            ( min-player ( if ( string-equal max-player 'black ) 'W 'B ) )
        )
        
        ;Coin parity heuristic
        ( * 100 ( / ( - ( count-pieces max-player board ) ( count-pieces min-player board ) )
                    ( + ( count-pieces max-player board ) ( count-pieces min-player board ) )
        ))
    )
)

 
#|
 | Function: corner-heuristic
 |
 | Description: This the corners and courner neighbors ownership heuristic, 
 |   which was adopted from the website mentioned in the static function 
 |   documentation.
 |
 | Parameters:
 |   board - Current board state
 |   max-player - Which player is trying to maximize their result
 |
 | Return:
 |   (corners closeCorners) - Scores for corners heuristic and corner neighbors
 |                            heuristic
 |#
( defun corner-heuristic ( board max-player )
    ( let 
        ;Local vars
        (
            ;Max player color
            ( max-player ( if ( string-equal max-player 'black ) 'B 'W ) )

            ;Min player color
            ( min-player ( if ( string-equal max-player 'black ) 'W 'B ) )

            ;Counters
            ( max-corners 0 )
            ( max-close-corners 0 )
            ( min-corners 0 )
            ( min-close-corners 0 )

            ;Positions of corners on board
            ( corners '( 0 7 56 63 ) )

            ;Placeholder for analyze results
            results
        )
        
        ;Loop over each corner
        ( dolist ( i corners )
            ;Analyze corner
            ( setf results ( analyze-corner board max-player min-player i ) )

            ;Save results of analyze
            ( setf max-corners ( + ( car results ) max-corners ) )
            ( setf max-close-corners ( + ( cadr results ) max-close-corners ) ) 
            ( setf min-corners ( + ( caddr results ) min-corners ) )
            ( setf min-close-corners ( + ( cadddr results ) min-close-corners ) )
        )

        ;Return scaled values for corners and close corners
        ( list
            ;Value for corners
            ( * 25 ( - max-corners min-corners ) )
            ;Value for close corners
            ( * -12.5 ( - max-close-corners min-close-corners ) )
        )
    )    
)

 
#|
 | Function: analyze-corner
 |
 | Description: This is a helper function for corner-heuristic. It examines the 
 |   given corner position for how many pieces are owned by the min and max 
 |   players for the corner and its neighbors.
 |
 | Parameters:
 |   board - Current board state
 |   max-player - Which player is trying to maximize their result
 |   min-player - Which player is trying to minimize their result
 |   corner - What is the position of the corner being examined
 |
 | Return:
 |   (max-corners max-close-corners min-corners min-close-corners)
 |      - How many corners and corner neighbors are owned by the max player and
 |        by the min player
 |#
( defun analyze-corner ( board max-player min-player corner )
    ( let 
        ;Local vars
        (
            ;List of positions neighboring corners
            ( closeCorners 
                ( cond
                    ( ( = corner 0 ) '( 1 8 9 ) )
                    ( ( = corner 7 ) '( 6 14 15 ) )
                    ( ( = corner 56 ) '( 48 49 57 ) )
                    ( ( = corner 63 ) '( 54 55 62 ) )
                )
            )

            ;Counters
            ( max-corners 0 )
            ( max-close-corners 0 )
            ( min-corners 0 )
            ( min-close-corners 0 )
        )

        ;Loop over all possibilities
        ( cond
            ;If max player owns corner
            ( ( string-equal max-player ( nth corner board ) )
                ( setf max-corners ( 1+ max-corners ) ) )

            ;If min player owns corner
            ( ( string-equal min-player ( nth corner board ) )
                ( setf min-corners ( 1+ min-corners ) ) )

            ;If noone owns corner, check corner neighbors
            ( t
                ;Loop over corner neighbors
                ( dolist ( i closeCorners )
                    ;Count who owns corner neighbors
                    ( if ( string-equal max-player ( nth i board ) )
                        ( setf max-close-corners ( 1+ max-close-corners ) )
                        ( setf min-close-corners ( 1+ min-close-corners ) )            
                    )
                )
            )
        )

        ;Return counters for corners and corner neighbors
        ( list max-corners max-close-corners min-corners min-close-corners )
    )
)

 
#|
 | Function: mobility-heuristic
 |
 | Description: This the mobility heuristic, which was adopted from the website
 |   mentioned in the static function documentation. It attempts to compare how
 |   many moves each player will after for the current board position.
 |
 | Parameters:
 |   board - Current board state
 |   max-player - Which player is trying to maximize their result
 |
 | Return:
 |   value - Score for coin heuristic for current board
 |#
( defun mobility-heuristic ( board max-player )
    ( let 
        ;Local vars
        ( 
            ( min-player ( if ( string-equal max-player 'black ) 'white 'black ) )
            max-moves
            min-moves 
        )

        ;Get number of moves for each player
        ( setf max-moves ( length ( get-moves max-player board ) ) )
        ( setf min-moves ( length ( get-moves min-player board ) ) )

        ;Calculate heuristic value
        ( if ( > max-moves min-moves )
            ;If max has more moves
            ( / ( * 100 max-moves ) ( + max-moves min-moves ) )

            ;If min has more moves
            ( / ( * -100 min-moves ) ( + max-moves min-moves ) )
        )
    )
)
