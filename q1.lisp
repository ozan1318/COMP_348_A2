( defun rev ( x acc ) ; funtion to reverse list x using empty accumulator list
  ( if ( null x )
      ( return-from rev acc )
      ( rev ( cdr x )
                  ( cons ( car x )
                         acc ) ) ) )

( defun without-last ( x ) ; function to return the list x without its last element, reverses x, takes its cdr, and reverses again
    ( rev ( cdr ( rev x '( ) ) ) '( ) )
    )

(defun sub-list ( x from &optional to )
    ( if ( and to from )
        ( if ( and ( > from 0 )  ( <= to ( list-length x ) )  ( < from to ) )
           ( if ( /= from 1 )
               ( sub-list ( cdr x )  ( - from 1 )  ( - to 1 ) )
               ( if ( = ( list-length x ) to )
                   ( return-from sub-list x )
                   ( sub-list (without-last x) from to ) ) )
           ( princ "ERROR: Your inputs are incorrect. Please try again." ) ) 
        ( if from
             ( if ( /= from 1 )
                 ( sub-list ( cdr x ) ( - from 1 ) )
                 ( return-from sub-list x) )
             ( princ "ERROR *** - EVAL/APPLY: Too few arguments" ) ) ) )