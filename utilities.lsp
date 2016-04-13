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


#|
 | Function: print-board
 |
 | Description:
 |   This function prints the board along with row & col numbers
 |
 |#
( defun print-board ()
    ;Prints column headers
    ( format t "  1 2 3 4 5 6 7 8~%" )
    
    ;Print each row of the board along with row numberp
    ( dotimes ( i 8 )
        ( format t "~d ~d ~d ~d ~d ~d ~d ~d ~d~%" 
            ( 1+ i )
            ( nth ( + 0 ( * i 8 ) ) *BOARD* )
            ( nth ( + 1 ( * i 8 ) ) *BOARD* )
            ( nth ( + 2 ( * i 8 ) ) *BOARD* )
            ( nth ( + 3 ( * i 8 ) ) *BOARD* )
            ( nth ( + 4 ( * i 8 ) ) *BOARD* )
            ( nth ( + 5 ( * i 8 ) ) *BOARD* )
            ( nth ( + 6 ( * i 8 ) ) *BOARD* )
            ( nth ( + 7 ( * i 8 ) ) *BOARD* )
        )
    )
)


#|
 | Function: play-game
 |
 | Description:
 |   
 |
 |#
( defun play-game ( player )
    
)