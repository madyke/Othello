#|
 |	Initialization
 |#
(defun othello-init ()
    ( defvar *BOARD* '( - - - - - - - -
                        - - - - - - - -
                        - - - - - - - -
                        - - - W B - - -
                        - - - B W - - -
                        - - - - - - - -
                        - - - - - - - -
                        - - - - - - - - ) ) 
)

(defun convertToPosition (rowCol)
    ;(format t "ConvertToPosition: ~s~%" rowCol)
    ;(format t "POS: ~s~%" (+ (* (1- (car rowCol)) 8) (1- (nth 1 rowCol))))
    (+ (* (1- (car rowCol)) 8) (1- (nth 1 rowCol)))
)

(defun convertToRowCol (pos)
    (let ( (rowCol nil) (row nil) (col nil) )
        (push (1+ (mod pos 8)) rowCol)
        (push (1+ (nth-value 0 (floor pos 8))) rowCol)
    )
)

(defun test-path (direction hasJumped label rowCol board)
    (let ( (localRowCol (copy-list rowCol)) )
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

        ;(format t "Direction: ~s localRowCol in test-path: ~s~%" direction localRowCol)

        (cond
            ;Fell off board
            ((OR (> (car localRowCol) 8) (> (nth 1 localRowCol) 8) (< (car localRowCol) 1) (< (nth 1 localRowCol) 1)) (return-from test-path nil))
            ;if test is true then we have already "jumped" an opposing tile
            ;and if there is a space then we have a valid move
            (
                (string-equal (nth (convertToPosition localRowCol) board) "-") 
		(if (eq hasJumped t)
                    (return-from test-path localRowCol)
                    (return-from test-path nil)
                )
            )
            ;If the square has the current player's tile then it's not valid
            ((eq (nth (convertToPosition localRowCol) board) label) (return-from test-path nil))
            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either blank or the opposing player's tile again
            (t (setf hasJumped t) (if(setf localRowCol (test-path direction hasJumped label localRowCol board)) (return-from test-path localRowCol) (return-from test-path nil)))
        )
    )
)

#|
 | 	This will check if the player has any available moves and
 | 	if so will return a list of available moves so we can check 
 | 	that against the move the player enters
 |#
(defun get-moves (player board)
    (let ( (moves '()) (label nil) (move nil) (validMove nil) )
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
        moves
    )
)
;TODO Clean this up
(defun flip-tiles (move player board)
    (let ( (pos (convertToPosition move)) (label nil) (test nil) )
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        (setf (nth pos board) label)
        (setf test (flip-path 'Up nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'UpRight nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'Right nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'DownRight nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'Down nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'DownLeft nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'Left nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
        (setf test (flip-path 'UpLeft nil label move board))
        (if (> (length test) 1)
            (setf board (car (last test)))
        )
       board
    )
)

(defun flip-path (direction hasJumped label move board)
    (let ( (localRowCol (copy-list move)) )
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
( defun print-results ()
    ;Print final board state
    ( print-board )
    
    ;Print how many pieces each player captured
    ( format t "~%Final Score:~%")
    ( format t "  BLACK: ~s" ( count-pieces "B" ) )
    ( format t "  WHITE: ~s" ( count-pieces "W" ) )
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
( defun count-pieces ( target )
    ( let 
        ;Local var - tracks number of occurences
        ( ( occur 0 ) )
        
        ;Loop over each spot on the board
        ( dotimes ( i ( list-length *BOARD* ) )        
            ;When board spot is occupied by player's pieces
            ( when ( string-equal target ( nth i *BOARD* ) ) ( setf occur ( 1+ occur ) ) )
        )
        
        ;Return number of occurences
        ( return-from count-pieces occur )
    )
)
