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

; Convers the board to a list of lists. Not currently used but 
; may be used for bound checking
(defun convert-board ()
    (let ((localBoard nil) (row nil))
        (dotimes (i (list-length *BOARD*))
             ;Add row to board
             (if (and (not (eq i 0)) (eq (mod i 7) 0))
                 (push localBoard row)
                 (setf row nil)
             )
             ;Add i to row
             (push row i)
        )
    )
)

;TODO Fix bounds checking. Probably rename hasJumped
(defun test-path (direction pos hasJumped label)
    (let ( (new-pos (+ pos direction)) )
        ;(format t "Entered test-path~%")
        ;(format t "Direction: ~s~%" direction)
        ;(format t "New-pos: ~s~%" new-pos)
        (cond
            ;Fell off board
            ((< new-pos 0) (return-from test-path nil))
            ;Fell off board
            ((> new-pos (list-length *BOARD*)) (return-from test-path nil))
            ;if test is true then we have already "jumped" an opposing tile
            ;and if there is a space then we have a valid move
            (
                (eq (nth new-pos *BOARD*) "-") 
                (if (test)
                    (return-from test-path t)
                    (return-from test-path nil)
                )
            )
            ;If the square has a the current player's tile then it's not valid
            ((eq (nth new-pos *BOARD*) label) (return-from test-path nil))
            ;The square is an opposing player's tile so we have to check the next
            ;position to see if it is either blank or the opposing player's tile again
            ;TODO Use something besides progn since Weiss doesn't like that
            (t (progn (setf hasJumped t) (test-path direction new-pos hasJumped label)))
        )
    )    
)

#|
 | 	This will check if the player has any available moves and
 | 	if so will return a list of available moves so we can check 
 | 	that against the move the player enters
 |#
(defun get-moves (player)
    (let ( (moves nil) (move nil) (valid nil) (label nil))
        (if (eq player 'black)
            (setf label 'B)
            (setf label 'W)
        )
        ;For each position in the board
        (dotimes (i (list-length *BOARD*))
            ;If the board position is for the current player
            (if (eq (nth i *BOARD*) label)
                ;check all 8 positions
                (cond
                    ;Checks each direction Up, UpRight, Right, DownRight,
                    ;Down, DownLeft, Left, UpLeft
                    ((test-path -8 i nil label) (setf valid t))
                    ((test-path -7 i nil label) (setf valid t))
                    ((test-path 1 i nil label) (setf valid t))
                    ((test-path 9 i nil label) (setf valid t))
                    ((test-path 8 i nil label) (setf valid t))
                    ((test-path 7 i nil label) (setf valid t))
                    ((test-path -1 i nil label) (setf valid t))
                    ((test-path -9 i nil label) (setf valid t))
                )
                (if (eq valid t)
                    ;Convert position to row column and add to list
                    ;TODO Fix progn
		    (progn (cons move (% i 8))
                    (cons move (floor (/ i 8)))
                    (cons moves move))
                )
            )
        )
        ;(format t "Moves: ~s~%" moves)
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
