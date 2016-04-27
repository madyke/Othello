#|
 | Function: play-PvP-game
 |
 | Description: This fucntion allows a player to play against another player.
 | The first player will be black and the second player will play white.
 | This function checks to see if there are no valid moves left for both players
 | in which case the game is over. If the player has valid moves then the move
 | entered by the player will be verified as a valid move and if so the game
 | board will be updated to represent their move.
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
                    ( not ( setf moves ( get-moves player *BOARD*) ) )
                    ;If previous player had no moves
                    ( not ( null prev-no-moves ) )
                )
                
                ;Game over, print info
                ( format t "~%~s has no available moves.~%~%GAME OVER~%~%" player )
                ( print-results *BOARD* )
                
                ;Ask for rematch
                ( format t "~%Would you like to play another match (y/n)?" )
                
                ;If user responds 'y', start new game
                ( when ( equal ( read ) 'y )
                    ( setf *BOARD* '( - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - W B - - -
                                      - - - B W - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - - )
                    )
                    ( play-PvP-game )
                )
            )

            ;Get all of the valid moves for the current player
            (setf moves (get-moves player *BOARD*))
        
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
                    ( print-board *BOARD*)
                    ( format t "~%What is ~s player's move [row col]? " player)
                
                    ;Get users move and store as a list
                    ( setf move ( list ( read ) ( read ) ) )

                    ;Check users entered move
                    (cond
                        ;If move was valid
                        ( ( find move moves :test #'equal )
                            ;Perform move and switch current player
                            ( setf *BOARD* (flip-tiles move player *BOARD*) )
                            ( if ( eq player 'black )
                                ( setf player 'white )
                                ( setf player 'black )
                            )
                        )
                        ;If move was invalid
                        ( t
                            ;Inform user
                            (format t "~%~s is not a valid move for ~s~%~%" move player)
                        )
                    )
                    
                    ;Mark that previous player had available moves
                    ( setf prev-no-moves NIL )
                )
            )
        )
    )
)


#|
 | Function: play-PvE-game
 |
 | Description: This function allows the player to play against the AI. If the
 | player did not enter which color they want to be then they are prompted if
 | they want to go first. If the user goes first they are black otherwise they
 | are white. The function will rotate between player moves and AI moves. If
 | both the player and AI have invalid moves then the game is over. Otherwise
 | if there are valid moves and it is the players turn the function will verify
 | that the move is valid before updating the game board. If it is the AI's turn
 | then make-move will be called to get the move the AI will make.
 |#
( defun play-PvE-game ( player )
    (let ( (moves nil) (human player) )
        ;If user has not selected their color
        ( when ( null human ) 
            ;Ask user if they want to go first
                ( format t "Would you like to go first (y/n)? " )

            ;If user responds 'y', set to black, else set to white
            ( if ( equal ( read ) 'y )
                ( setf human 'black )
                ( setf human 'white )
            )
        )

        ;Print game information
        ( format t "~%OK! You will be playing ~d. When asked for your " human )
        ( format t "move, please enter the row and column in which you would " )
        ( format t "like to place a ~d stone. Remember, you must outflank " human )
        ( format t "at least one White stone, or forfeit your move.~%~%" )
        
        (setf player 'black)
    
        ;Play game
        ( do
            ;Local vars
            ( ( prev-no-moves NIL ) )
        
            ;Termination Condition & termination statements
            (
                ;Termination condition
                ( and 
                    ;If current player has no moves
                    ( not ( setf moves ( get-moves player *BOARD*) ) )
                    ;If previous player had no moves
                    ( not ( null prev-no-moves ) )
                )
                
                ;Game over, print info
                ( format t "~%~s has no available moves.~%~%GAME OVER~%~%" player )
                ( print-results *BOARD* )
                
                ;Ask for rematch
                ( format t "~%Would you like to play another match (y/n)?" )
                
                ;If user responds 'y', start new game
                ( when ( equal ( read ) 'y )
                    ( setf *BOARD* '( - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - W B - - -
                                      - - - B W - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - - )
                    )
                    ( play-PvE-game human )
                )
            )
            
            ;DEBUG
            ( format t "~%~%Moves:~s~%~%" moves )
       
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
                    (cond
                        ( (string-equal human player)                       
                            ;Print board and request next move
                            ( print-board *BOARD*)
                            ( format t "~%What is ~s player's move [row col]? " player)
                
                            ;Get users move and store as a list
                            ( setf move ( list ( read ) ( read ) ) )

                            ;Check users entered move
                            (cond
                                ;If move was valid
                                ( ( find move moves :test #'equal )
                                    ;Perform move and switch current player
                                    ( setf *BOARD* (flip-tiles move player *BOARD*)) 
                                    ( if ( eq player 'black )
                                        ( setf player 'white )
                                        ( setf player 'black )
                                    )
                                )
                                ;If move was invalid
                                ( t
                                    ;Inform user
                                    (format t "~%~s is not a valid move for ~s~%~%" move player)
                                )
                            )
                    
                            ;Mark that previous player had available moves
                            ( setf prev-no-moves NIL )
                        )
                        (t 
                            (format t "Player: ~s~%" player)
                            (print-board *BOARD*)
                            (setf move (make-move *BOARD* player 4))
                            ( setf *BOARD* (flip-tiles move player *BOARD*)) 
                            ( if ( eq player 'black )
                                 ( setf player 'white )
                                 ( setf player 'black )
                            )
                        )
                    )
                )
            )
        )
    )
)


#|
 | Function: play-EvE-game
 |
 | Description: This function will allow the AI to play against itself. The game will
 | flip between black and white players and call the make-move function on each
 | player's turn to allow the AI to make it's move.
 |#
( defun play-EvE-game ()
    (let ( (moves nil) )
        ;Print game information
        (setf player 'black)
    
        ;Play game
        ( do
            ;Local vars
            ( ( prev-no-moves NIL ) )
        
            ;Termination Condition & termination statements
            (
                ;Termination condition
                ( and 
                    ;If current player has no moves
                    ( not ( setf moves ( get-moves player *BOARD*) ) )
                    ;If previous player had no moves
                    ( not ( null prev-no-moves ) )
                )
                
                ;Game over, print info
                ( format t "~%~s has no available moves.~%~%GAME OVER~%~%" player )
                ( print-results *BOARD* )
                
                ;Ask for rematch
                ( format t "~%Would you like to watch another match (y/n)?" )
                
                ;If user responds 'y', start new game
                ( when ( equal ( read ) 'y )
                    ( setf *BOARD* '( - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - W B - - -
                                      - - - B W - - -
                                      - - - - - - - -
                                      - - - - - - - -
                                      - - - - - - - - )
                    )
                    ( play-EvE-game )
                )
            )
                   
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
                    (format t "~%Player: ~s~%~%" player)
                    (print-board *BOARD*)
                    (setf move (make-move *BOARD* player 1))
                    ( setf *BOARD* (flip-tiles move player *BOARD*)) 
                    ( if ( eq player 'black )
                         ( setf player 'white )
                         ( setf player 'black )
                    )
                )
            )
        )
    )
)
