let rec cif_max n = if n <= 99 then (max (n mod 10) (n/10)) else max (n mod 10) (cif_max (n/10));; 

cif_max 192534;;


