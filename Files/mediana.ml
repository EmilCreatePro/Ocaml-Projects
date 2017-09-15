let med a b c = if a > b && a > c && b > c then b else min (max a b) c;;
med 3 8 5