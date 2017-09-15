module Int = struct
type t = int
let compare = compare
end
module IS = Set.Make(Int);;           

let m1 = IS. add 1 (IS.singleton 2);;

IS.elements m1;;

let rec m1 i n = if i = n then IS.singleton i else IS.add i (m1 (i+1) n);;

IS.elements (m1 0 10);;