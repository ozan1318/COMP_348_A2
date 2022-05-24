( defun depth ( x )
  ( if ( atom x ) ; check if x is an atom
      0 ; if atom, return 0
      ( 1+ ( reduce #'max ( mapcar #'depth x ) :initial-value 0 ) ) ) )
; if not atom, call depth for every element in x with `mapcar #'depth x`. After the depth for every element is recursively found, 
; use `reduce #'max` to eliminate all but the highest value from `mapcar #'depth`. Then add 1 to get the result.