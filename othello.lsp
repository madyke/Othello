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
    ;Load program files
    ( load 'utilities.lsp )

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
    
    ;Play game
    ( play-game )
)


;Script commands for program when run from command line
( othello ( car *ARGS* ) )