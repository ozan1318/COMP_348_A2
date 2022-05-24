( defun rev ( x acc ) ; funtion to reverse list x using empty accumulator list
  ( if ( null x )
      ( return-from rev acc )
      ( rev ( cdr x )
                  ( cons ( car x )
                         acc ) ) ) )

( defun without-last ( x ) ; function to return the list x without its last element, reverses x, takes its cdr, and reverses again
    ( rev ( cdr ( rev x '( ) ) ) '( ) )
    )

(defun sub-list2 ( x from &optional to ) ;  x is the list, to is optional
    ( if ( and to from ) ; if both from and to are entered
        ( if ( and ( > from 0 )  ( <= to ( list-length x ) )  ( < from to ) ) ; check if both are valid inputs in the correct order
             ( if ( /= from 1 ) ; check if from = 1
                  ( sub-list2 ( cdr x )  ( - from 1 )  ( - to 1 ) ) ; if /= 1, recursively call sub-list2 with cdr x and the inputs adjusted
                  ( if ( = ( list-length x ) to ) ; if from = 1, check if to = list-length
                       ( return-from sub-list2 x ) ; if yes return the list
                       ( sub-list2 (without-last x) from to ) ) ) ; else, recursively call sub-list2 after removing the final element
             ( if ( /= from 1 ) ; if the inputs are invalid, check if from = 1
                  ( if ( <= from 0 ) ; if from /= 1, check if its less than 1
                       ( if ( = to 1 ) ; if less than 1, check for the special case to = 1. return the car of x as a list
                            ( list ( car x ) ) ; if yes, return the car of x as a list
                            ( sub-list2 x 1 to ) ) ; if no, recall sub-list2 with from = 1 as the behaviour is the same
                       ( sub-list2 ( cdr x ) ( - from 1 ) to ) ) ; if from > 1, recursively call sub-list2 with the lists cdr and from - 1
                  ( return-from sub-list2 x ) ) ) ; if from = 1, return current list
         ( if from ; if to hasnt been entered, check if from is enteres
             ( if ( <= from 1 ) ; check if from is enetered but <= 1
                  ( return-from sub-list2 x ) ; if from <= 1, return list
                  ( sub-list2 ( cdr x ) ( - from 1 ) ) ) ; if from > 1, recursively call sub-list2 with the cdr of x and from - 1
             ( princ "ERROR *** - EVAL/APPLY: Too few arguments" ) ) ) )