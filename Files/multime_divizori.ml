module Int = struct
type t = int
let compare = compare
end
module IS = Set.Make(Int);;  

let multDiv n =
     let rec div i = 
         if i <= (n/2)
            then if n mod i = 0 then IS.add i (div (i+1)) else div (i+1)
               else IS.singleton n;
       in div 1 ;;

(*IS.elements (multDiv 204);;*)

