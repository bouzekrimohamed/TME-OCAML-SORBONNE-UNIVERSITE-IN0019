let rec bin_to_bin (i : int list):int=
match i with 
|[]->raise (Invalid_argument "empty list")
|[h]->h
|h::t->h+2*(bin_to_bin t);;


let rec int_to_bin (n : int):int list=
if n=0 then [0] 
else if n=1 then []
else (n mod 2)::(int_to_bin (n/2)) ;;

let rec comp_bin (l : int list)(n:int):int list =

if (List.length l)>n then  raise (Invalid_argument "comp_bin")
else if (List.length l)=n then l
 
else (comp_bin (l@[0]) n) ;;

let rec genere_list(n : int):int list =
if n<2 then []
else (genere_list (n-1))@[n];;

let rec elimine (l : int list) (n : int) : int list=
match l with 
|[]->[]
|h::t-> if (h mod n)<>0 then h::(elimine t n) else (elimine t n);;

let rec ecreme (l : int list) : int list =
match l with 
|[]->[]
|h::t->h:: ecreme(elimine t h);;

let crible (n : int) : int list=
ecreme(genere_list (n-1));;
