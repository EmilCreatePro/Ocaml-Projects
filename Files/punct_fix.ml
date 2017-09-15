let f x = 3 * x - 10;;

let rec fix f x =
  let nxt = f x in
 if nxt = x then x else fix f nxt;;

fix f 100;;