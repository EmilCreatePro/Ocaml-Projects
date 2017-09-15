let rec prod_cif n = if n <= 9 then n else n mod 10 * prod_cif (n/10);;

prod_cif 416;;