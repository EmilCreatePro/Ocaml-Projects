(*http://stackoverflow.com/questions/13826810/fast-fibonacci-recursion*)

let fib n = (*n este indicele termenului din sirul lui fibonacci*)
    let rec ifib i a b = (* i - parcurg numerele pana la n ; a va fi mereu al (n - 2) termen din fibonacci si b (n - 1) termen*) 
        if i = n then b (*daca i a ajuns la n-ul termen din fibonacci returneaza suma celor doi termeni dinainte 
                         - adica b care i-a fost data valoarea a+b la apel adica (n-1)+(n-2) *)
        else ifib (i + 1) b (a + b) (* i+1 este urmatorul temren din sirul lui fibonacci, b este suma ultimilor 
                                       doi termeni dinainte si a + b este al (i+1) numar din sir fibonacci*)
    in
    ifib 0 0 1;; (* este prima apelare care se face pentru sirul lui fibonacci *)

fib 50;;