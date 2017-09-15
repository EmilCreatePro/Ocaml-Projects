let list1 = 0 :: [1;2;3];;
open Printf;;
(* tiparire lista *)
List.iter (Printf.printf "%d ") list1;;
List.iter print_int list1;;

let lista = [2;52;21;23;4;5];;

let list_goala = [];;

(* verificare lista daca e goala *)

let verif_lista = function
  | [] -> printf "lista e goala"
  | h :: t -> printf "lista unu e goala" ;;

verif_lista list_goala;;

(* gasirea unui element in lista *)

let rec find_x x = function
  | [] -> printf "Elementul %d NU a fost gasit" x;
  | h :: t -> if x = h then printf "Elementul a fost gasit" else find_x x t;;

find_x 4 lista;;

(* lungimea unei liste *)

let rec len r = function
  | [] -> r;
  |  _ :: t -> len (r+1) t;;

len 0 lista;;

(* SAU!!!! *)

List.length list1;;

(* conditie de tiparire a liste *)

let list_par = [1;2;3;4;5;6;7;8;9;10];;

List.filter (fun x -> x mod 2 = 0) list_par;;

(* SAU *)

List.filter ((<)3) list_par;;

(* aplicarea unei reguli la toate elemntele listei *)

List.map ((+)2) list_par;;

(* facerea numereleo impare pare *) 

List.map (fun x -> if x mod 2 <> 0 then x + 1 else x) list_par;;

(* inversarea unei liste *)

List.rev list_par;;



