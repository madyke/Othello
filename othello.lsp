
(setf *board* '(- - - - - - - -
		- - - - - - - -
		- - - - - - - -
		- - - W B - - -
		- - - B W - - -
		- - - - - - - -
		- - - - - - - -
		- - - - - - - -))

#|
 |	Initialization
 |#
(defun othello-init ()

)

#|
 | 	This is the move function used for the AI in the tournament
 |#
(defun make-move (board player ply)

)

#|
 | 	This will check if the player has any available moves and
 | 	if so will return a list of available moves so we can check 
 | 	that against the move the player enters
 |#
(defun check-moves (player)

)

( defun othello ( &optional ( player nil ) )
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
    
    ( format t "~s~%" player )
    
    
)


;Script commands for program when run from command line
( othello ( car *ARGS* ) )