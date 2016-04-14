#|
 | Function: play-PvP-game
 |
 | Description:
 |   
 |
 |#
( defun play-PvP-game ()
    ;Print game information
    ( format t "~%OK! The first player will be playing BLACK, the second " )
    ( format t "player will be playing WHITE. When asked for your move, " )
    ( format t "please enter the row and column in which you would like to " )
    ( format t "place a stone. Remember, you must outflank at least one " )
    ( format t " of your opponent's stones, or forfeit your move.~%" )
    
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
    
    ( print-board )
    
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