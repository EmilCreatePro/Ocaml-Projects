(*let rec div_max n acc = if n mod acc = 0 then acc else div_max n (acc-1);;

div_max 203 202;;*)

let div_max n =
  let rec div acc = 
    if n mod acc = 0 
        then acc else div (acc-1)
   in div (n-1);;
 
 (*div_max 203;;*)
 
 let rec div_2_max f x y = if f(x) = f(y) then f(x) else (if x > y then div_2_max f (x-1) y else div_2_max f x (y-1));;
 
div_2_max div_max 18 27;;