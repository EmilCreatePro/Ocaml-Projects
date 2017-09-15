type bform = B of bool | V of int | Neg of bform |  And of bform*bform |  Or of bform*bform;;

let rec find_var = function
  | B _ -> raise Not_found
  | V s -> s
  | Neg f -> find_var f
  | And (f1,f2) | Or (f1,f2) -> try find_var f1 with Not_found -> find_var f2;;

find_var (And (V 5,V 2));;
find_var (And (B true, Or (Neg (B true),V 7)));;