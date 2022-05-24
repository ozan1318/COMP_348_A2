( defun rev ( x acc ) ; funtion to reverse list x using empty accumulator list
  ( if ( null x )
      ( return-from rev acc )
      ( rev ( cdr x )
                  ( cons ( car x )
                         acc ) ) ) )

( defun without-last ( x ) ; function to return the list x without its last element, reverses x, takes its cdr, and reverses again
    ( rev ( cdr ( rev x '( ) ) ) '( ) )
    )

( defun sub-list3 ( x from &optional to ) ;  x is the list, to is optional
    ( if ( and to from ) ; if both from and to are entered
         ( if ( < from to ) ; check input order
             ( if ( and ( > from 0 )  ( <= to ( list-length x ) ) ) ; check if both are valid inputs
                  ( if ( /= from 1 ) ; check if from = 1
                       ( sub-list3 ( cdr x )  ( - from 1 )  ( - to 1 ) ) ; if /= 1, recursively call sub-list3 with cdr x and the inputs adjusted
                       ( if ( = ( list-length x ) to ) ; if from = 1, check if to = list-length
                            ( return-from sub-list3 x ) ; if yes return the list
                            ( sub-list3 (without-last x) from to ) ) ) ; else, recursively call sub-list3 after removing the final element
                  ( if ( <= to ( list-length x ) ) ; if the inputs are invalid, check if to <= list x's length
                       ( if ( = to 1 ) ; if to <= list x's length, if-and was triggered false by (< from 0). check for the special case to = 1
                            ( list ( car x ) ) ; if to = 1, return the car of x as a list
                            ( sub-list3 x 1 to ) ) ; to /= 1, therefore call sub-list3 with from = 1 as this results in the same behaviour
                       ( sub-list3 x from ) ) ) ; if to > length of list x, call sub-list 3 without to
             ( rev ( sub-list3 x to from ) '( ) ) ) ; if ordered incorrectly, recall the function with the list and the inputs reversed
         ( if from ; if to hasnt been entered, check if from is enteres
             ( if ( /= from 1 ) ; check if from is enetered but /= 1
                 ( if ( <= from 0 ) ; check if from < 0
                      ( return-from sub-list3 x ) ; if from < 0, return list
                      ( sub-list3 ( cdr x ) ( - from 1 ) ) ) ; if from > 1, recursively call sub-list3 with the lists cdr and from - 1
                  ( return-from sub-list3 x ) ) ; if from = 1, return list
             ( princ "ERROR *** - EVAL/APPLY: Too few arguments" ) ) ) )