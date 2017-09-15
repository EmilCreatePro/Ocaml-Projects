let rec nr_cif n = if n <= 9 then 1 else 1 + nr_cif (n/10);;

nr_cif 123456789;;