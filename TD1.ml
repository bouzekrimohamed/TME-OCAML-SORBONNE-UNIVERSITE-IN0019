let rec sum_chiffres (n : int) : int =
  if n = 0 then 0
  else n mod 10 + sum_chiffres (n / 10);;
  
 let rec nb_chiffres (n : int) : int =
  if n < 10 then 1
  else 1 + nb_chiffres (n / 10);;

  
let rec less_divider (i : int) (n : int) : int =
  if i > n / 2 then 0
  else if n mod i = 0 then i
  else less_divider (i + 1) n;;

let rec prime (n : int) : bool =
  if n=1 then false 
  else if (less_divider 2 n)=0 then true
  else false;;

let rec next_prime (n : int) : int =
  if prime n then n
  else next_prime (n + 1);;
  
let rec nth_prime(n : int):int =
  if n=0 then 2 else
  
  

  
  



