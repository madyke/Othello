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
    
    ;Ask user to select game type
    ( format t "The following game types are available:~%" )
    ( format t "  1. Player vs Player~%" )
    ( format t "  2. Player vs Computer~%" )
    ( format t "  3. Computer vs Computer~%" )
    ( format t "Please enter the number of the game type you want to play: " )
    ( setf game-type ( read ) )
    
    ;If user made invalid selection, request again
    ( do ()
        ;Termination condition - user selectd 1, 2, or 3
        (( or
            ( equal game-type 1 )
            ( equal game-type 2 )
            ( equal game-type 3 )
         ))
        
        ;Request different choice
        ( format t "That is not an acceptable game type. " )
        ( format t "Please choose 1, 2, or 3: ")
        ( setf game-type ( read ) )
    )
    
    ;Start requested game type
    ;( cond
    ;    ( equal )
    ;)
    
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
    ( play-game player )
)


;Script commands for program when run from command line
( othello ( car *ARGS* ) )