(load "utilities.lsp")

#|
 | Function: play-PvP-game
 |
 | Description:
 |   
 |
 |#
( defun play-PvP-game ()
    (let ( (player 'black) (moves nil))
        ;Print game information
        ( format t "~%OK! The first player will be playing BLACK, the second " )
        ( format t "player will be playing WHITE. When asked for your move, " )
        ( format t "please enter the row and column in which you would like to " )
        ( format t "place a stone. Remember, you must outflank at least one " )
        ( format t " of your opponent's stones, or forfeit your move.~%~%" )
    
        ;Loop player moves
        ( do
            ;Local vars
            ( ( prev-no-moves NIL ) )
        
            ;Termination Condition & termination statements
            (
                ;Termination condition
                ( and 
                    ;If current player has no moves
                    ( not ( setf moves ( get-moves player ) ) )
                    ;If previous player had no moves
                    ( not ( null prev-no-moves ) )
                )
                
                ;Game over, print info
                ( format t "~%~s has no available moves.~%~%GAME OVER" player )
            )

            ;TODO Restructure this fucntion so get-moves isn't run on failure
            (setf moves (get-moves player))
        
            ;Check if current player has available moves
            ( cond
                ;If current player has no moves
                ( ( null moves )
                    ;Inform player
                    ( format t "~%~s has no available moves.~%" player )
                    
                    ;Swap current player
                    (if (eq player 'black)
                        (setf player 'white)
                        (setf player 'black)
                    )
                    
                    ;Mark that previous player had no available moves
                    ( setf prev-no-moves 1 )
                )
                
                ;Else player has at least one move
                ( t
                    ;Print board and request next move
                    ( print-board )
                    ( format t "~%What is ~s player's move [row col]? " player)
                
                    ;Get users move and store as a list
                    ( setf move ( list ( read ) ( read ) ) )

                    ;Check users entered move
                    (cond
                        ;If move was valid
                        ( ( find move moves :test #'equal )
                            ;Perform move and switch current player
                            ( flip-tiles move player ) 
                            ( if ( eq player 'black )
                                ( setf player 'white )
                                ( setf player 'black )
                            )
                        )
                        ;If move was invalid
                        ( t
                            ;Inform user
                            (format t "~s is an incorrect move~%" move)
                        )
                    )   
                )
            )
        )
    )
)


#|
 | Function: play-PvE-game
 |
 | Description:
 |   
 |
 |#
( defun play-PvE-game ( player )
    ;Set players piece color
    ( cond
        ;If no starting player given
        ( ( null player )
            ;Ask user if they want to go first
            ( format t "Would you like to go first (y/n)? " )
            
            ;If user responds 'y', set to black, else set to white
            ( if ( equal ( read ) 'y )
                ( setf player 'black )
                ( setf player 'white )
            )
        )
        ;If starting player given
        ( t
            ;If user entered "BLACK", set to black, else set to white
            ( if ( string-equal player "BLACK" )
                ( setf player 'black )
                ( setf player 'white )
            )
        )
    )
    
    ;Print game information
    ( format t "~%OK! You will be playing ~d. When asked for your " player )
    ( format t "move, please enter the row and column in which you would " )
    ( format t "like to place a ~d stone. Remember, you must outflank " player )
    ( format t "at least one White stone, or forfeit your move.~%~%" )
    
    ;Loop player moves
    ( do
        ;Local vars
        ( ( move-num 0 ( 1+ move-num ) ) )
        
        ;Termination Condition - PLACEHOLDER
        ( ( > move-num 10 ) )
        
        ;Print board and request next move
        ( print-board )
    )
    
    ( format t "~%GAME OVER~%")
)


#|
 | Function: play-EvE-game
 |
 | Description:
 |   
 |
 |#
( defun play-EvE-game ()
    
)
