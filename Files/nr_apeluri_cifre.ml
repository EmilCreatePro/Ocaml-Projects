let f n = if n mod 2 = 0 then n/2 else 3 * n + 1;;

let rec p = function
  | 1 -> 0
  | n -> 1 + p(f n);;

#trace p;;

p(f 18);;