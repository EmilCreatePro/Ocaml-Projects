let lista = [1;2;3;4;5;6;7];;

(* parcuregerea unei liste *)

open Printf;;

let rec parcurg = function 
  | [] -> printf "am ajuns la capatul liste"
  |  h :: t -> (printf "%d " h); parcurg t;;

parcurg lista;;

(* laborator 3 exercitiul 3 *)

let rec nthElement n i = function
  | [] -> failwith "Nu s-a gasit al n-lea element" 
  (*| n<0 -> failwith "NU exista elemente pe pozitii negative!" *)
  | h :: t -> if i = n then h else nthElement n (i+1) t;;

(*nthElement 3 0 lista;;*)

let rec firstn i n = function
  | [] -> failwith "toate elemnetele listei au fost puse!"
  (*| h :: t -> h :: firstn (i+1) n t;;*)
  | h :: t -> if i <= n then h :: firstn (i+1) n t else [];;

(*firstn 0 3 lista;;*)

(*functia random*)
Random.self_init();;

let rec aleator n b = if n <> 0 then (printf "%d " (Random.int (b-1)); (aleator (n-1) b)) else printf " -> gata";;

(*aleator 4 20;;*)


(*laborator 3 exercitiul 4*)
let rec constrList n b = if n <> 0 then (Random.int (b-1)) :: constrList (n-1) b  else [];;

(*constrList 4 50;;*)

(*List.fold_right*)

let sum_elem_lista = function
  | [] -> failwith "e goala lista"
  |  h :: t -> List.fold_right (fun h t -> h + t) t h;;

(*sum_elem_lista [1;2;3];;*)

let prod_elem_lista = function
  | [] -> failwith "lista e goala"
  | h :: t -> List.fold_left (fun h t -> t * h) h t;;

(*prod_elem_lista [1;2;3;4];;*)


(*inversarea elementelor listei cu fold_right*)
let rev list = List.fold_right (fun h t -> t @ [h]) list [];;

(*rev lista;;*)

(*List.filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6;7;8;9;10;11;12];;*)

(* laborator 3 exercitiul 9 *) 

let x = List.hd [2;3;4;5];;


let rec nr_cifre = function
  | 0 -> 0
  | n -> 1 + nr_cifre(n/10);; 

let rec pow a = function
  | 0 -> 1
  | n -> a * (pow a (n-1));; 


let rec lton = function
  | [] -> 0
  | h :: t -> h * (pow 10 (List.length t)) + lton t;;

(* lton [4;2;6];; *)

let verif_lista list = if (List.hd [list]) = [] then printf "lista goala" else printf "lista are ceva";;

(*verif_lista [1;2;3];;*)

List.filter ((<)3) [1;2;3;4;5;6;7];;

(* (fun x -> if x mod 2 = 0 then printf "numar par" else printf "numar impar") 10;; *)

(* laborator 3 exercitiul 5 *)
let filter_right list = List.fold_right (fun h acc -> if h mod 2 = 0 then h::acc else acc) list [];; 

(*filter_right [1;2;3;4;5;6;7;8];;*)

(* laborator 3 exercitiul 6 a) *)
let countif list = List.length(List.fold_left (fun acc e -> if e > 2 then e::acc else acc) [] list);;  

(*countif [1;2;3;4;5;6;7;8];;*)


(* laborator 3 exercitiul 6 b) *)

let sumif list = List.fold_left (fun acc e -> if e > 2 then acc + e else acc) 0 list;;

(*sumif [3;2;1;5;0;3];;*)

(* laborator 3 exercitul 13 *)

let rec separate = function
  | x::y::tail ->
        let a,b = separate tail in
        x::a, y::b
  | x::[] -> [x],[]
  | [] -> [],[];;

(* separate [1;2;34;4;5;6];; *)

(* separare lista *)

let rec separ_list = function
  | [] -> [],[]
  | x :: [] -> [x],[]
  | x :: y :: coada -> let a,b = separ_list coada in x :: a, y :: b;;

separ_list [1;2;3;4;5;6;7];;