#|
 | Program: Othello
 | Authors: Matt Dyke, Christian Sieh
 | Class: CSC 447 - Artificial Intelligence
 | Instructor: Dr. Weiss
 | Due Date: April 21, 2016
 |
 | Description:
 |
 | Input:
 | Output:
 | Compilation instructions: Run in CLisp on Linux or Windows
 | Usage:
 |#

;;------------------------------ Global Variables ------------------------------;

( defvar *BOARD* '( - - - - - - - -
                    - - - - - - - -
                    - - - - - - - -
                    - - - W B - - -
                    - - - B W - - -
                    - - - - - - - -
                    - - - - - - - -
                    - - - - - - - - ) ) ;Othello board state

;--------------------------------- Functions ----------------------------------;

#|
 | Function: othello
 |
 | Description:
 |
 | Parameters:
 |   &optional ( player nil ) - Color of player's pieces
 |
 |#
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
)


;Script commands for program when run from command line
( othello ( car *ARGS* ) )