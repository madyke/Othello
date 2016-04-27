#|
 | Function: converToPosition
 |
 | Description: This function takes in a 2 items list made up of a row and column
 | such as (3 4). This list is then converted to a position in the graph.
 |#
(defun convertToPosition (rowCol)
    ;(format t "ConvertToPosition: ~s~%" rowCol)
    ;(format t "POS: ~s~%" (+ (* (1- (car rowCol)) 8) (1- (nth 1 rowCol))))
    (+ (* (1- (car rowCol)) 8) (1- (nth 1 rowCol)))
)

#|
 | Function: convertToRowCol
 |
 | Description: This function takes in a position such 7 and converts it to
 | a row column list like (1 8).
 |#
(defun convertToRowCol (pos)
    (let ( (rowCol nil) (row nil) (col nil) )
        (push (1+ (mod pos 8)) rowCol)
        (push (1+ (nth-value 0 (floor pos 8))) rowCol)
    )
)

#|
 | Function: test-path
 |
 | Description: This is a recursive function takes in a direction such
 | as Up and it uses that to adjust the rowCol list passed in and in the
 | case it would sutract 1 from the first value in the rowCol list in oreder
 | to move up one row in the board. After the localRowCol is changed we then
 | test the value at that position in the board. This function checks if the
 | position caused us to fall off the board, if the position is an empty
 | space, if the position contains our tile, or if it's the opponent's
 | tile then we have to recursively check in that direction until
 | we either fall off the board or we reach an empty space. If we
 | reach an empty space after jumping over an opponent's tile then
 | we know this direction has a valid move.
 |#
(defun test-path (direction hasJumped label rowCol board)
    (let ( (localRowCol (copy-list rowCol)) )
        ;Change the localRowCol depending ont he direction provided
        (cond 
            ((eq direction 'Up) (setf (car localRowCol) (1- (car localRowCol))))
            ((eq direction 'UpRight) (setf (car localRowCol) (1- (car localRowCol))) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'Right) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'DownRight) (setf (car localRowCol) (1+ (car localRowCol))) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'Down) (setf (car localRowCol) (1+ (car localRowCol))))
            ((eq direction 'DownLeft) (setf (car localRowCol) (1+ (car localRowCol))) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
            ((eq direction 'Left) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
            ((eq direction 'UpLeft) (setf (car localRowCol) (1- (car localRowCol))) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
        )

        (cond
            ;Fell off board
            ((OR (> (car localRowCol) 8) (> (nth 1 localRowCol) 8) (< (car localRowCol) 1) (< (nth 1 localRowCol) 1)) nil)

            ;if test is true then we have already "jumped" an opposing tile
            ;and if there is a space then we have a valid move
            (
                (string-equal (nth (convertToPosition localRowCol) board) "-") 
		(if (eq hasJumped t)
                    localRowCol
                    nil
                )
            )

            ;If the square has the current player's tile then it's not valid
            ((eq (nth (convertToPosition localRowCol) board) label) nil)

            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either blank or the opposing player's tile again
            (t (setf hasJumped t) 
                (if(setf localRowCol (test-path direction hasJumped label localRowCol board)) 
                    localRowCol 
                    nil
                )
            )
        )
    )
)

#|
 | Function: get-moves
 | 
 | Description: This function will check if the player has any available moves and
 | if so will return a list containing those moves. This is accomplished by going
 | through each position on the board, checking if that board is owned by the
 | current player and then calling test-path on each of the 8 directions to see
 | if this valid positions to jump to from this position.
 |#
(defun get-moves (player board)
    (let ( (moves '()) label move validMove )
        ;Set the label depending on the player so we can check against the board
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        ;For each position in the board
        (dotimes (i (list-length board))
            ;If the board position is for the current player
            (when (eq (nth i board) label)
                (setf move (convertToRowCol i))
                ;Checks each direction Up, UpRight, Right, DownRight,
                ;Down, DownLeft, Left, UpLeft
                (if (setf validMove (test-path 'Up nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'UpRight nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'Right nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'DownRight nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'Down nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'DownLeft nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'Left nil label move board)) (push validMove moves))
                (if (setf validMove (test-path 'UpLeft nil label move board)) (push validMove moves))
            )
        )
        ;Delete duplicates so we don't have to deal with the same move from different positions
        (delete-duplicates moves :test #'equal)
    )
)

#|
 | Function: flip-tiles
 |
 | Description: This function will take a move provided, change that position
 | to be owned by the player, and then attempt to flip tiles in all 8 directions
 | by calling flip-path. If the path did flip tiles then results will be greater
 | than 1 and we can update the board to the returned value. 
 |#
(defun flip-tiles (move player board)
    (let 
        ((pos (convertToPosition move)) 
          label 
          results 
          (directions (list 'Up 'UpRight 'Right 'DownRight 'Down 'DownLeft 'Left 'UpLeft))
        )
	;Set the label based on the player so we can compare against the board
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
	;Set the current position to the player
        (setf (nth pos board) label)

	(dolist (direction directions)
            (setf results (flip-path direction nil label move board))
            (if (> (length results) 1)
                (setf board (car (last results)))
            )
        )
       board
    )
)

#|
 | Function: flip-path
 |
 | Description: This function takes the direction provided and attempts to flip
 | tiles along that path. This is accomplished by checking if the tile in that
 | direction is either not on the board, if it's our tile, if the tile is a dash,
 | or if the tile is an opposing player's tile in which case we recursively
 | call this function to continue a long the path until we either find a tile
 | that matches the player or we fall off the board. If we have found a tile
 | that belongs to our player and we have jumped over an opposing players
 | tile then we can flip tiles along this path so we return the localRowCol
 | and set the tile at that position to be the player. Finally we also return
 | the board so we can set the new board state in flip-tiles.
 |#
(defun flip-path (direction hasJumped label move board)
    (let ( (localRowCol (copy-list move)) )
        ;Update localRowCol based on the direction provided
        (cond 
            ((eq direction 'Up) (setf (car localRowCol) (1- (car localRowCol))))
            ((eq direction 'UpRight) (setf (car localRowCol) (1- (car localRowCol))) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'Right) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'DownRight) (setf (car localRowCol) (1+ (car localRowCol))) (setf (nth 1 localRowCol) (1+ (nth 1 localRowCol))))
            ((eq direction 'Down) (setf (car localRowCol) (1+ (car localRowCol))))
            ((eq direction 'DownLeft) (setf (car localRowCol) (1+ (car localRowCol))) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
            ((eq direction 'Left) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
            ((eq direction 'UpLeft) (setf (car localRowCol) (1- (car localRowCol))) (setf (nth 1 localRowCol) (1- (nth 1 localRowCol))))
        )
        
        
        (cond
            ;Fell off board
            ((OR (> (car localRowCol) 8) (> (nth 1 localRowCol) 8) (< (car localRowCol) 1) (< (nth 1 localRowCol) 1)) (return-from flip-path nil))

            ;if test is true then we have already "jumped" an opposing tile
            ;and if there is our tile then we have a valid move
            (
                (eq (nth (convertToPosition localRowCol) board) label) 
		(if (eq hasJumped t)
                    (progn (setf (nth (convertToPosition localRowCol) board) label) (list localRowCol board))
                    (return-from flip-path nil)
                )
            )

            ;If the square has a dash then it's not valid
            ((string-equal (nth (convertToPosition localRowCol) board) "-") (return-from flip-path nil))

            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either our tile or the opposing player's tile again
            (t (setf hasJumped t) 
                (if(flip-path direction hasJumped label localRowCol board)
                    (progn
                        (setf (nth (convertToPosition localRowCol) board) label) 
                        (list localRowCol board)
                    )
                    (return-from flip-path nil)
                )
            )
        )
    )
)

#|
 | Function: print-board
 |
 | Description:
 |   This function prints the board along with row & col numbers
 |
 |#
( defun print-board (board)
    ;Prints column headers
    ( format t "  1 2 3 4 5 6 7 8~%" )
    
    ;Print each row of the board along with row numberp
    ( dotimes ( i 8 )
        ( format t "~d ~d ~d ~d ~d ~d ~d ~d ~d~%" 
            ( 1+ i )
            ( nth ( + 0 ( * i 8 ) ) board )
            ( nth ( + 1 ( * i 8 ) ) board )
            ( nth ( + 2 ( * i 8 ) ) board )
            ( nth ( + 3 ( * i 8 ) ) board )
            ( nth ( + 4 ( * i 8 ) ) board )
            ( nth ( + 5 ( * i 8 ) ) board )
            ( nth ( + 6 ( * i 8 ) ) board )
            ( nth ( + 7 ( * i 8 ) ) board )
        )
    )
)


#|
 | Function: print-results
 |
 | Description:
 |   This function prints the results after a game ends
 |
 |#
( defun print-results ( board )
    ;Print final board state
    ( print-board *BOARD* )
    
    ;Print how many pieces each player captured
    ( format t "~%Final Score:~%")
    ( format t "  BLACK: ~s" ( count-pieces "B" board ) )
    ( format t "  WHITE: ~s" ( count-pieces "W" board ) )
)


#|
 | Function: count-pieces
 |
 | Description:
 |   This function counts the number of pieces the specified player has
 |
 | Parameters:
 |   
 |
 |#
( defun count-pieces ( target board )
    ( let 
        ;Local var - tracks number of occurences
        ( ( occur 0 ) )
        
        ;Loop over each spot on the board
        ( dotimes ( i ( list-length board ) )        
            ;When board spot is occupied by player's pieces
            ( when ( string-equal target ( nth i board ) ) ( setf occur ( 1+ occur ) ) )
        )
        
        ;Return number of occurences
        ( return-from count-pieces occur )
    )
)
