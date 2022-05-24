( defun flat ( x )
    ( cond ( ( null x ) nil )
        ( ( atom x ) ( list x ) )
        ( t ( loop for a in x appending ( flat a ) ) ) ) )

( defun nodup ( x )
    ( remove-duplicates x :from-end t ) )

( defun only-nums ( x ) 
    ( remove-if-not #'numberp x ))

(defun flatten-nums-nodup ( x )
    ( only-nums ( nodup ( flat x ) ) ) )

( flatten-nums-nodup '( 1 2 ( 3 1 ) ( a 2.5 ) ( 2 4.5 ) ( ( 1 2 ) ) ) )