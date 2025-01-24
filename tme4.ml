type 'a btree = Empty | Node of 'a * 'a btree * 'a btree
let bt_sujet = Node(5, Node(2,Node(1,Empty,Empty),Node(4,Node(3,Empty,Empty),Empty)),Node(6,Empty,Node(7,Empty,Node(8,Empty,Empty))))
let bt_1 = Node(2,Node(4,Empty,Empty),Empty) 
let bt_2 = Node(4,Node(6,Empty,Empty),Empty)
let bt_3 = Node(4,Node(5,Empty,Empty),Empty)
let bt_4 = Node(4,Node(3,Empty,Empty),Empty)
let bt_5 = Node(5,Node(2,Node(1,Empty,Empty),Node(4,Empty,Empty)),Node(6,Empty,Node(7,Empty,Empty)))
let bt_6 = Node(5,Node(2,Node(1,Empty,Empty),Node(4,Empty,Empty)),Node(3,Empty,Node(7,Empty,Empty)))
let bt_7 = Node(5,Node(2,Node(1,Empty,Empty),Node(4,Empty,Empty)),Node(7,Empty,Node(6,Empty,Empty)))
let bt_8 = Node(3,Node(1,Empty,Node(2,Empty,Node(2,Empty,Node(2,Empty,Empty)))),Node(5,Empty,Node(31,Node(18,Empty,Empty),Node(42,Empty,Empty))))
let b_test = Node(2,Node(2,Node(7,Empty,Empty),Empty),Node(4,Empty,Node(1,Empty,Empty)))
let bt_99=Node(5,Node(2,Node(1,Empty,Empty),Node(8,Empty,Empty)),Node(6,Empty,Node(7,Empty,Empty)));;



//EXERCICE 1 :

//QUESTION 1 : 

let rec lt_btree (t:'a btree) (x:'a) : bool = 
  match t with
  |Empty -> true
  |Node(e,g,d)-> x>e && lt_btree g x && lt_btree d x ;;

//QUESTION 2 : 

let rec ge_btree (t:'a btree) (x:'a) : bool =
  match t with 
  |Empty -> true 
  |Node(e,g,d)-> x<=e && (ge_btree g e) && (ge_btree d e);;

//QUESTION 3 : 
  
  
let rec is_abr (bt:'a btree) : bool=
  match bt with 
  |Empty->true
  |Node(e,g,d)-> if (lt_btree g e) && (ge_btree d e) then 
    (is_abr g) && (is_abr d)
    else false ;;

//EXERCICE 2 :

//QUESTION 1 :

let rec mem (bt: 'a btree) (x:'a) : bool =
  match bt with 
  |Empty->false
  |Node(e,g,d)-> if e=x then true else
    if x > e then (mem d x) else
      (mem g x);;


//EXERCICE 3 

//QUESTION 1 :

let rec insert (bt : 'a btree)(x:'a) : 'a btree =
  match bt with 
  |Empty->Node(x,Empty,Empty)
  |Node(e,g,d)-> if x>e then Node(e,g,(insert d x)) else 
    if x<e then Node(e,(insert g x),d) else 
    Node(e,g,d);;(* bt*)

(*QUESTION 2 : *)

let rec abr_of_list_v2 (l:'a list) : 'a btree =
match l with 
|[]->Empty
|h::t-> (insert (abr_of_list_v2 t)h)
 
let arb_of_list (l:'a list): 'a btree =
  List.fold_left insert Empty l ;;


(*EXERCICE 4 : *)

(*QUESTION 1 : *)

let rec list_of_abr (bt:'a btree) : 'a list =
match bt with 
|Empty -> []
|Node(e,g,d)->  list_of_abr (g)@ [e] @ list_of_abr (d);;

(*question 2*)

let abr_sort (l:'a list) : 'a list =
  (list_of_abr (abr_of_list l)  );;




let rec lt_btree (t:'a btree) (x:'a) : bool=
  match t with 
  |Empty->true
  |Node(a,g,d)->(a<x && (lt_btree g x) && (lt_btree d x));;

let rec  is_abr (bt:'a btree) : bool=
  match bt with 
  |Empty-> true 
  |Node(a,g,d)-> (lt_btree g a ) && (ge_btree d a) && (is_abr d) && (is_abr g);;

let rec  insert (bt:'a btree) (x:'a) : 'a btree=
  match bt with 
  |Empty->Node(x,Empty,Empty)
  |Node(a,g,d)-> if (x>a) then insert d x else insert g x ;; 