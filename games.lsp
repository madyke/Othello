#|
 | Function: play-PvP-game
 |
 | Description:
 |   
 |
 |#
( defun play-PvP-game ()
    
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
    
    ;Print game info
    ( format t "~%OK! You will be playing ~d. When asked for your " player )
    ( format t "move, please enter the row and column in which you would " )
    ( format t "like to place a Black stone. Remember, you must outflank " )
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