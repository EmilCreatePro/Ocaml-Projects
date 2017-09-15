[1;3;2;7;3;9];;

let rec mem x = function 
| [] -> false
| h :: t -> 2 = h || mem 2 t