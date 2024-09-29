(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  (0,Z);
  (1,(S Z));
  (2,(S (S Z)))
]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec nat_of_int (n : int) (res : nat) : nat = 
    if n = 0 then res else
      nat_of_int (n - 1) (S(res))
  in
  nat_of_int n Z

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  (Z,0);
  ((S Z),1);
  ((S (S Z)),2)
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int = 
  let rec int_of_nat (n : nat) (sum : int) : int = 
    match n with 
    | Z -> sum
    | S n -> int_of_nat n (sum + 1)
  in
  int_of_nat n 0

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z,Z),Z);
  ((Z,(S Z)),(S Z));
  ((((S Z)),((S(S Z)))),(S (S (S Z))))
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  match n with
  | Z -> m
  | S n -> S(q1c_add n m)


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times(Const (-1.0),e)

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus(e1, (q2a_neg e2))

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = 
  let rec pow e1  p : exp = 
    match p with
    | Z -> (Const 1.0)
    | S n -> Times(e1, pow e1 n)
  in pow e1 p


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  (( 1.0 ,Plus(Const 2.0,Const 2.0) ),  4.0 );
  (( 1.0 ,Plus(Var,Const 2.0) ),  3.0 );
  (( 1.0 ,Times(Var,Const 2.0) ),  2.0 );
  ( (6.0, Times( Div( Var , Const 2.0 ) , Var ) ) , 18.0);
  (( 6.0 ,Div(Var,Const 2.0) ),  3.0 ); 
]

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with 
  | Plus (x,y) -> (eval a x) +. (eval a y)
  | Times (x,y) -> (eval a x) *. (eval a y)
  | Div (x,y) -> (eval a x) /. (eval a y)
  | Const x -> x
  | Var -> a


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  ( Const(5.0) ,Const 0.0);
  ( Var ,Const 1.0);
  ( Plus(Times(Var,Const 3.0),Const 4.0) , Plus (Plus (Times (Const 1., Const 3.), Times (Var, Const 0.)), Const 0.)); 
  (Div(Var,Plus(Var,Const 1.)) , Div
     (Plus (Times (Const 1., Plus (Var, Const 1.)),
            Times (Const (-1.), Times (Var, Plus (Const 1., Const 0.)))),
      Times (Plus (Var, Const 1.), Plus (Var, Const 1.))) );
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with
  | Plus (e1,e2) -> Plus((diff e1) , (diff e2))
  | Times (e1,e2) -> Plus( Times( (diff e1) , e2 ) ,  Times(  e1 ,  (diff e2) ) )
  | Div (e1,e2) -> Div( (q2b_minus (Times( (diff e1) , e2 )) (Times(  e1 ,  (diff e2) )) ) , 
                        Times(e2,e2) )
  | Const _ -> Const 0.0
  | Var -> Const 1.0
    
    
    
    
