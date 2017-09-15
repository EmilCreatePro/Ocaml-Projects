module S = Set.Make(String);;

let m1 = S.add "ana" (S.add "are" (S.singleton "mere"));;

(*nr de elemnte ale multimii*)
S.cardinal m1;;

(*daca un elemnt se afla in lista*)
S.mem "lia" m1;;

(*afisarea elemtelor unei multimi*)
S.elements m1;;

let m2 = S.add "scrt" (S.singleton "luuuuung");;

S.elements m2;;

open Printf
let print_strset s = print_char '{'; S.iter (printf " %s") s; print_endline " }"
;;

S.elements (S.union m1 m2);;

let ord_alf = S.add "bbb"(S.add "aaa"(S.add "eee"(S.add "fff"(S.add "ccc"(S.singleton "ddd")))));;

S.elements ord_alf;;

print_strset ord_alf;;

let nr_cres = S.add "2"(S.add "5"(S.add "1"(S.add "4"(S.add "3"(S.singleton "6")))));;

S.elements nr_cres;;

let list = [4;2;7;6;1;5];;

