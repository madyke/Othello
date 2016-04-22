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

(defun test-path (direction hasJumped label rowCol)
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
                (string-equal (nth (convertToPosition localRowCol) *BOARD*) "-") 
		(if (eq hasJumped t)
                    (return-from test-path localRowCol)
                    (return-from test-path nil)
                )
            )
            ;If the square has the current player's tile then it's not valid
            ((eq (nth (convertToPosition localRowCol) *BOARD*) label) (return-from test-path nil))
            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either blank or the opposing player's tile again
            (t (setf hasJumped t) (if(setf localRowCol (test-path direction hasJumped label localRowCol)) (return-from test-path localRowCol) (return-from test-path nil)))
        )
    )
)

#|
 | 	This will check if the player has any available moves and
 | 	if so will return a list of available moves so we can check 
 | 	that against the move the player enters
 |#
(defun get-moves (player)
    (let ( (moves '()) (label nil) (move nil) (validMove nil) )
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        ;For each position in the board
        (dotimes (i (list-length *BOARD*))
            ;If the board position is for the current player
            (when (eq (nth i *BOARD*) label)
                (setf move (convertToRowCol i))
                ;Checks each direction Up, UpRight, Right, DownRight,
                ;Down, DownLeft, Left, UpLeft
                (if (setf validMove (test-path 'Up nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'UpRight nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'Right nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'DownRight nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'Down nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'DownLeft nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'Left nil label move)) (push validMove moves))
                (if (setf validMove (test-path 'UpLeft nil label move)) (push validMove moves))
            )
        )
        moves
    )
)

(defun flip-tiles (move player)
    (let ( (pos nil) (label nil) (row nil) (col nil) )
        (setf row (* (- (car move) 1) 8))
        (setf col (- (nth 1 move) 1))
        (setf pos (+ row col))
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        (setf (nth pos *BOARD*) label)
    )
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
