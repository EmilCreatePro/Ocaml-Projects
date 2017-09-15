(* functii partiale*)

(fun x -> x + 1) 3 ;;

let f2 x y = 2*x + y - 1 ;;

let f_from_f2 x = f2 1 x;;

(* f_from_f2 3;; *)


(* conjecura lui Collatz*)
let rec collatz n pasi = if n = 1 then pasi else if n mod 2 = 0 then collatz(n / 2) (pasi + 1) else collatz (3 * n + 1) (pasi + 1);;

(*#trace collatz;;

collatz 10 0;;*)

let rec prog_arit a1 r rang_termen = match rang_termen with
  | 1 -> a1
  | _ -> prog_arit (a1 + r) r (rang_termen - 1);;

(*prog_arit 1 1 4;;*)

let rec prog_geo b1 q rang_termen = match rang_termen with
  | 1 -> b1
  | _ -> prog_geo (b1 * q) q (rang_termen - 1);;

(*prog_geo 3 2 5;; *)

(* aria unui triunghi *)

let aria a b c = 
    let p = (a +. b +. c)/.2.0
 in sqrt (p *.(p -. a) *. (p -. b) *. (p -. c));;

(*aria 3.0 3.0 3.0;; *)

(* crearea unei liste din cifrele unui numar *)

let cif_from_list n = 
  let rec lst n list = if n < 10 then (n mod 10) :: list else (n mod 10) :: (lst (n / 10) list)
        in lst n [];;

(*cif_from_list 23125;; *)


(* crearea unei liste din cifrele unui numar prin List.fold_left *)

let make_lst_fold lst = List.fold_left (fun numar lista -> (numar * 10) + lista) 0 lst;; 

(*make_lst_fold (cif_from_list 23125);;*)

(* cel mai mic divzor comun *)

let rec cmmdc (a, b) = match (a, b) with
  | (a, 0) | (0, a) -> a 
  | (a ,b) -> if (a > b) then cmmdc (a-b, b) else cmmdc (a, b-a);;

(* cmmdc (18, 12);; *)

(* testeaza daca primele doua elemente ale unei liste sunt egale *)

let list_egal = [1;1;4;2;2;5;2];;

let list_neegal = [2;5;2;6];;

let list_apr_vid = [1];;

let is_list_egal lst = match lst with
  | h1 :: h2 :: t -> h1 = h2
  | _ -> failwith "lista nu are 2 elemente sau e vida";;

(*
is_list_egal list_egal;;
is_list_egal list_neegal;;
is_list_egal list_apr_vid;;
*)

(*determinare daca un element este in lista*)

let rec mem lst x = match lst with
  | [] -> false 
  | h :: t -> h = x || mem t x;;

(*mem list_egal 4;; *)

(* lungimea unei liste *)

let rec nr_elemente lst = match lst with
  | [] -> 0
  | h :: t -> 1 + nr_elemente t;;

(* nr_elemente list_egal;; *)

(* lungimea unei liste mai bine *)

let lenght_left lst = List.fold_left (fun acc lista -> acc + 1) 0 lst;; (* varianta fold_left*)

(*lenght_left list_egal;;*)

let lenght_right lst = List.fold_right (fun lista acc -> acc + 1) lst 0;;

(*lenght_right list_egal;;*)

(* realizarea unui tail_recirsdive pro_arit *)

let rec pro_arit n = if n = 0 then 1 else 2 + pro_arit (n-1);; (* varianta ineficienta *)

(* pro_arit 10;; *) (* nu va mere *)

let pro_arit_eficienta n = 
   let rec acumulate m acc = 
     if m = 0 then acc else acumulate (m - 1) (acc + 2)
in acumulate n 1;;

(*pro_arit_eficienta 1000000;; *) (* varianta eficienta *)

(* afisarea unei liste cu List.iter *)

let list_ordonata = [1;2;3;4;5;6;7;8];;

open Printf;;


(* afisarea uneil iste cu finctia iter *)
let iter lst =(printf "["); List.iter (printf "%d; ") lst; printf "]";;

(*iter list_ordonata;;*)

(*concatenarea unei liste*)

(* a) intr-un mod indeficient*) 

let concat_list lst1 el = lst1 @ [el];;

(*concat_list list_ordonata 10;;*) (* faorte ineficient *)

let rec concat_list_eficient_in_spate lst el = match lst with
  | [] -> [el]
  | h :: t -> h :: (concat_list_eficient_in_spate t el);;

(*concat_list_eficient_in_spate list_ordonata 10;; *)

let concat_list_eficient_in_fata lst el = match lst with
  | [] -> [] 
  | h :: t -> el :: h :: t;;

(*concat_list_eficient_in_fata list_ordonata 10;;*)

(* lungimea unei liste cu ajutorul potrivirii de tipare *)

let len lst = 
   let rec len2 lst2 r = match lst2 with
     | [] -> r
     | h :: t -> len2 t (r+1)
 in len2 lst 0;;
     
(*len list_ordonata;;*)

List.exists ((<)5) list_ordonata;;

List.filter ((>)5) list_ordonata;;

(*implementarea functiei filter cu List.fold_orice*)

let filter_left lst = List.fold_left (fun acc el -> if(el > 4) then (el :: acc) else acc) [] lst;;

filter_left list_ordonata;;

let filter_right lst = List.fold_right (fun el acc -> if(el > 4) then (el :: acc) else acc) lst [];;

filter_right list_ordonata;;

(* aspect practic la functia map *)

let add2_to_list lst = List.map (fun x -> x + 2) lst;;

(*add2_to_list list_ordonata;; *)

List.rev_map (fun x -> x) list_ordonata;;

(* implementarea functiei map*)

let f1 x = x + 2;;

let rec map lst f = match lst with
  | [] -> []
  | h :: t -> (f h) :: (map t f);;

(* map list_ordonata f1;; *)

(* aplicatii utile al lui List.fold... *)

(* maximul unei liste*)

let max_lst lst = match lst with
  | [] -> failwith "lista e goala"
  | h :: t -> List.fold_left min h t;;

(*max_lst list_neegal;; *)

(* inversarea unei liste *)
 let rev lst = List.fold_left (fun acc el_list -> el_list :: acc) [] lst;;

(*rev list_ordonata;; *)

(* MULTMI !!!!!!!!!!!!!!!!!!*)

module Int = struct 
 type t = int
 let compare = compare
end

module IS = Set.Make(Int);;

(* generarea unei multimi de la numarul a la b*)

let generate_multime_from_a_to_b a b =
    let rec ajut mult a1 b1 =
    if a1 = b1 then IS.add a1 mult else (IS.add a1 (ajut mult (a1+1) b1))
  in ajut IS.empty a b;;

(*IS.elements (generate_multime_from_a_to_b 2 10);; *)

let multime_random = generate_multime_from_a_to_b 1 8;;

(* aduna cu 2 fiecare elemnt din multime*)

let add2 m = IS.fold (fun el_mult acc -> IS.add (el_mult + 2) acc) m IS.empty;;

IS.elements (add2 multime_random);;

(*afisarea unei multlimi*)

let afis m = printf "{"; IS.iter (printf "%d; ") m; printf "}";;

(*afis multime_random;;*)

(* construirea unei multimi dintr-o lista *)

let list_to_kill = [4;2;1;5;6;2;4;7];;

let create_set_from_list lst = List.fold_left (fun acc el_lst -> IS.add el_lst acc) IS.empty lst;;

(*IS.elements (create_set_from_list list_to_kill);;*)


(* construirea unei multimi dintr-o lista cu patter_matching *)

let rec create_set_match lst = match lst with
  | [] -> IS.empty 
  | h :: t -> IS.add h (create_set_match t);;

(*IS.elements (create_set_match list_to_kill);;*)

(* parcurgerea unei multimi cu fold *)

let create_list_from_set m = IS.fold (fun el_set acc -> el_set :: acc) m [];; (* la multimi fold-ul are acc in dreapta mereu!*)

(*List.rev_map (fun x-> x) (create_list_from_set multime_random);;*)

IS.elements (IS.filter ((<)3) multime_random);;

(* implemenatrae functiei filter cu ajutorul lui fold *)

let filter m = IS.fold (fun el_set acc -> if(el_set > 5) then (IS.add el_set acc) else acc) m IS.empty;;

(*afis (filter multime_random);; *)

(* DICTIONARE!!!! *)

module DS = Map.Make(String);;

module DI = Map.Make(Int);;

let dictionar = DS.add "x" 5 (DS.add "y" 6 (DS.add "z" 7 (DS.add "a" 1 (DS.add "b" 2 (DS.add "t" 3 (DS.add "p" 1 DS.empty))))));;

(* mereu se va utiliza try _ with Not_found -> failwth "mesaj" *)

let is m k = try DS.find k m with Not_found -> 0;;

(*is dictionar "v";; *)

let dictionar_cu_chei_lafel = DS.add "x" 5 (DS.add "y" 6 (DS.add "z" 7 (DS.add "y" 1 (DS.add "y" 2 (DS.add "x" 3 (DS.add "z" 1 DS.empty))))));;

(*DS.bindings dictionar_cu_chei_lafel;;*)

(* sa faca suma valorilor cheilor unui dictionar *)

let sum_val dict = DS.fold (fun k v acc -> v + acc) dict 0;;

(*sum_val dictionar_cu_chei_lafel;;*)

let add_2_to_val dicto = DS.map (fun k v -> v + 2) dicto;;

(*DS.bindings (add_2_to_val dictionar_cu_chei_lafel);;*)

let list_char = ["a";"a";"b";"b";"a"];;

(* de cate ori apare o litera intr-o lista *)

let aparitii lst = 
  List.fold_left (fun dict_apar el_lst -> 
                    if (DS.mem el_lst dict_apar)=false then (DS.add el_lst 1 dict_apar) 
                      else DS.add el_lst ((is dict_apar el_lst)+1) dict_apar ) DS.empty lst;;
                                                         
DS.bindings (aparitii list_char);;                                     
  

