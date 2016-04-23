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
    (let ( (pos (convertToPosition move)) (label nil) (row nil) (col nil) )
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        (setf (nth pos *BOARD*) label)
        (flip-path 'Up nil label move)
        (flip-path 'UpRight nil label move)
        (flip-path 'Right nil label move)
        (flip-path 'DownRight nil label move)
        (flip-path 'Down nil label move)
        (flip-path 'DownLeft nil label move)
        (flip-path 'Left nil label move)
        (flip-path 'UpLeft nil label move)
    )
)

(defun flip-path (direction hasJumped label move)
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
                (eq (nth (convertToPosition localRowCol) *BOARD*) label) 
		(if (eq hasJumped t)
                    (progn (setf (nth (convertToPosition localRowCol) *BOARD*) label) (return-from flip-path localRowCol))
                    (return-from flip-path nil)
                )
            )
            ;If the square has a dash then it's not valid
            ((string-equal (nth (convertToPosition localRowCol) *BOARD*) "-") (return-from flip-path nil))
            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either blank or the opposing player's tile again
            (t (setf hasJumped t) 
                (if(flip-path direction hasJumped label localRowCol)
                    (progn
                        (setf (nth (convertToPosition localRowCol) *BOARD*) label) 
                        (return-from flip-path localRowCol) 
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
