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
    ;Call initialization to get board and other files
    (othello-init)
    
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
    ( cond
        ;User selected PvP game
        ( ( equal game-type 1 )
            ( play-PvP-game )
        )
        ;User selected PvE game
        ( ( equal game-type 2 )
            ( play-PvE-game player )
        )
        ;User selected EvE game
        ( ( equal game-type 3 )
            ( play-EvE-game )
        )
    )
)


#|
 | Function: othello-init
 |
 | Description:
 |
 |#
( defun othello-init ()
    ;Load program files
    ( load "utilities.lsp" )
    ( load "games.lsp" )
    ( load "minimax.lsp" )

    ;Define global board
    ( defvar *BOARD* '( - - - - - - - -
                        - - - - - - - -
                        - - - - - - - -
                        - - - W B - - -
                        - - - B W - - -
                        - - - - - - - -
                        - - - - - - - -
                        - - - - - - - - )
    ) 
)


;Script commands for program when run from command line
( othello ( car *ARGS* ) )