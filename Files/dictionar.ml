module M = Map.Make(String);;

(*let m = M.add "a" 2 M.empty;;*)

let m1 = M.add "c" "1" (M.add "c" "5" (M.add "b" "2" (M.singleton "d" "3")));;

M.bindings m1;;

(*let rec matc list n = match list with
  | [] -> (Printf.printf "%d" n)
  | h::t -> matc t (n+1);;

matc [1;2;3;4;5;6;9] 0;;
*)

(*tema problema 2 curs*)

(*module S = Set.Make(String);*)

open Printf;;

let sir_list = ['a';'b';'c';'b';'d';'a'];;

let rec afisare lista = match lista with
  | [] -> []
  | h :: t -> (printf "%c " h); afisare t;;

afisare sir_list;;

(*let count_ch 

let sum mt = M.fold (fun k e v -> e + v) mt 0;;

let m2 = M.empty;;

M.is_empty m2;; *)

(*let finder m c = try M.find m c with Not_found -> M.empty*)
                
(*M.mem "z" m1;;*)

(*module M = Map.Make(String);;*)

let get m k = try M.find k m with Not_found -> 0;;

let transf lst = List.fold_left (fun m x-> if (M.mem x m)=false then (M.add x 1 m) else M.add x ((get m x)+1) m) M.empty lst;;

let p = transf ["a";"b";"a";"c"];;

M.bindings p;;

let mc = M.add "c" 6(M.add "d" 2(M.add "c" 3 (M.add "c" 4(M.add "a" 5 (M.add "z" 7 (M.add "c" 0 (M.singleton "m" 2)))))));;

M.bindings mc;;

let what m k = try M.find k m with Not_found -> 0;;

what mc "d";;

M.mem "c" mc;;



let is m k = try M.find k m with Not_found -> 0;;
let count_ch list = List.fold_left (fun m x -> if (M.mem x m)=false then (M.add x 1 m) else M.add x ((is m x)+1) m) M.empty list;;
let dict = count_ch ["a";"b";"a";"b";"a";"a"];;

M.bindings dict;;

(*exercitiul 4 laborator 4*)

module Int = struct
  type t = int
  let compare = compare
end
module IS=Set.Make(Int);;

let set1 = IS.add 1(IS.add 2 (IS.add 3 (IS.add 5 (IS.add 6 (IS.add 7 (IS.singleton 4))))));;

IS.elements set1;;

let part set = IS.fold (fun x (sp,si) -> if x mod 2 = 0 then (IS.add x sp,si) else (sp,IS.add x si)) set (IS.empty, IS.empty)   ;;

part set1;;

(*exercitiul 4 laborator 6*)

let rec submultimi = function
      | [] -> [[]]
      | x :: xs -> 
         let ps = submultimi xs in
          ps @ List.map (fun sm -> x :: sm) ps;;

submultimi [1;2;3];;


 module S = Set.Make(String);;

    module SS = Set.Make(S);;
    
let submultimi xs = 
      S.fold (fun x ps -> 
          SS.fold (fun ss -> SS.add (S.add x ss)) ps ps) xs 
             (SS.singleton S.empty);;

(*submultimi ["1";"2";"3"];;*)

 module Superset(T : Set.OrderedType) = struct
      module S = Set.Make(T)

      module SS = Set.Make(S)
      let of_set xs = 
        S.fold (fun x ps -> 
            SS.fold (fun ss -> SS.add (S.add x ss)) ps ps) xs
               (SS.singleton S.empty)
    end

module Superset_of_strings = Superset(String);;
open Superset_of_string;;  
of_set (S.of_list ["1"; "2"; "3"]) |> SS.elements |> List.map S.elements;;

