let rec cif_min n = if n <= 99 then (min (n mod 10) (n/10)) else min (n mod 10) (cif_min (n/10));; 

cif_min 14563;;

