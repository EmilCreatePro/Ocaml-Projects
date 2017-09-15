let f x = 2 * x - 1
let g x = x - 3
let op ( sgn ) x y = (sgn) x y

op ( + ) (f 1) (g 1);;