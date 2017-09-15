let rec sum_cif n = if n <= 9 then n else n mod 10 + sum_cif (n/10);;
  sum_cif 111110;;