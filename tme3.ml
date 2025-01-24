//EXERCICE 1 :

//question 1 :

let rec is_in (e : 'a) (l : 'a list):bool =
    match l with 
    |[]->false
    |h::t-> if (h=e) then true else is_in e t;;

//question 2 :

let add_elem (e : 'a) (l : 'a list) : 'a list =
    if (is_in e l)=false then e::l else l ;;

//EXERCICE 2  :
 //question 1 :


let rec  is_subset_rec (l1 : 'a list) (l2 : 'a list) : bool =
    match l1 with 
    |[]->true
    |h::t-> if is_in h l2 then is_subset_rec t l2 else false ;;


let is_subset (l1 : 'a list)(l2 : 'a list):bool=
    List.for_all (fun x -> is_in x l2) l1;;

let eq_set (l1: 'a list) (l2: 'a list) : bool=
    is_subset(l1 l2) && is_subset(l2 l1);;

//EXERCICE 3 
//QUESTION 1 :

let rec intersection_rec (l1 : 'a list) (l2 : 'a list) : 'a list =
    match l1 with 
    |[]->[]
    |h::t-> 
        if List.mem h l2 then 
            h::intersection_rec t l2 
        else 
            intersection_rec t l2 ;;

let intersection (l1 : 'a list )(l2 : 'a list): 'a list=
    List.filter(fun x -> is_in x l2) l1 ;; 

//EXERCICE 4 :

//QUESTION 1 :

let rec union_rec_v2 (l1 : 'a list)(l2: 'a list): 'a list =
    match l1 with 
    |[]->l2
    |h::t->
        if (List.mem h l2 = false )then 
            h:: union_rec_v2 t l2 
        else 
            union_rec_v2 t l2;;

let rec union_rec (l1 : 'a list)(l2: 'a list): 'a list =
    match l1 with 
    |[]->l2
    |h::t->union_rec t (add_elem h l2);;

let union_left (l1:'a list)(l2:'a list): 'a list =
    List.fold_left(fun acc x -> (add_elem x acc) )l2 l1;;


let union_right (l1: 'a list) (l2: 'a list) : 'a list =
    List.fold_right (fun x acc -> add_elem x acc) l1 l2;;

//EXERCICE 5 

//QUESTION 1 :

let make_pairs (x : 'a) (l : 'b list) : ('a * 'b) list =
    List.map (fun s -> (x,s)) l ;;

//QUESTION 2 :

let rec product_rec (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
    match l1 with 
    |[]->[]
    |h::t->(make_pairs h l2)@(product_rec t l2) ;;

    //QUESTION 3 

let rec product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
    List.flatten(List.map (fun x -> (make_pairs x l2))l1);;

//EXERCICE  6 

//QUESTION 1 :

let rec powerset_rec (l : 'a list) : 'a list list =
    match l with
    | [] -> [[]] 
    | h :: t ->
        let ps_t = powerset_rec t in  
        let with_h = List.map (fun subset -> h :: subset) ps_t in  
        ps_t @ with_h ;; 
  
let powerset (l : 'a list) : 'a list list =
    List.fold_left (fun acc x ->acc @ (List.map (fun subset -> x :: subset) acc)) [[]] l
          
 ;;




 let rec achat_produit (prod : string * int) (stock : (string * int) list):(string * int) list * int=
    let (x,y)=prod in 
    match stock with 
     |[]->([],y)
     |(h1,h2)::t-> 
                let (n,m)=achat_produit prod t in  
                if (h1=x) then
                  if y<h2 then ((h1,h2-y)::n,0)
                     else (n,y-h2)
                else ((h1,h2)::n,m );;
             