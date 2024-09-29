(* Write some SUCCESS test cases for `infer` *)
let infer_tests : ((ctx * exp) * tp) list = [
  (([],I 5),Int);
  (([],B true),Bool);
  (([("x",Int)],Var "x"),Int);
  (([("x",Int)],If(Primop(LessThan, [Var("x"); I(0)]), B(false), B(true))), Bool);
  (* 3 *)
  (([],ex1),Arrow ([Int; Int], Int));
  (* 4 *)
  (([],ex2),Arrow ([], Bool));
  (([("y",Int)],Let("x",I 5,Primop(LessThan,[Var "x";Var "y"]))) , Bool); 
  (* 6 *)
  (([],Apply(ex1,[I 1;I 2])),Int);
  (* 7 *)
  (([],Apply(ex2,[])),Bool); 
  (* 1 *)
  (([],Rec("x",Int,Primop(Plus,[Var "x";I 6]))),Int);
  (([],Rec("f",Arrow([Int],Int),Fn([("x",Int)],Primop(Times,[Var "x";Apply(Var "f",[(Primop(Minus,[Var "x";I 1]))])])))),Arrow ([Int], Int));
  (* 2 *)
  (([("y",Int)],Fn([("x",Int)],Primop(Plus,[Var "x";Var "y"]))),Arrow ([Int], Int));
  (* 5 *)
  (([("y",Int)],Apply(Fn([("x",Int)],Primop(Plus,[Var "x";Var "y"])),[I 1])),Int);
]

(* Q1: `infer_op` - Type Inference in Primitive Operations *)

(* [infer_op] returns the type of a primitive operation *)
let rec infer_op (op : primop) (ts : tp list) : tp =
  match op with
  | Equals -> if not(List.length ts = 2) then raise ArityMismatch 
      else
      if (List.nth ts 0) = (List.nth ts 1) then Bool 
      else
        raise TypeMismatch
          
  | LessThan -> if not(List.length ts = 2) then raise ArityMismatch 
      else
      if (List.nth ts 0) = Int && (List.nth ts 1) = Int then Bool
      else 
        raise TypeMismatch
          
  | Plus -> if not(List.length ts = 2) then raise ArityMismatch 
      else
      if (List.nth ts 0) = Int && (List.nth ts 1) = Int then Int
      else 
        raise TypeMismatch
          
  |  Minus -> if not(List.length ts = 2) then raise ArityMismatch 
      else
      if (List.nth ts 0) = Int && (List.nth ts 1) = Int then Int
      else 
        raise TypeMismatch
          
  | Times -> if not(List.length ts = 2) then raise ArityMismatch 
      else
      if (List.nth ts 0) = Int && (List.nth ts 1) = Int then Int
      else 
        raise TypeMismatch
          
  | Negate -> if not(List.length ts = 1) then raise ArityMismatch 
      else
      if (List.nth ts 0) = Int then Int
      else
        raise TypeMismatch
(* Q2: `infer` - General Type Inference *) 

(* [infer] returns the type of an expression in a context *)
let rec infer (ctx : ctx) (e : exp) : tp =
  (* helper *)
  let rec get_type l = 
    match l with 
    | [] -> []
    | (a,b)::xs -> b :: (get_type xs) in 
  
  match e with
  | I _ -> Int
    
  | B _ -> Bool
    
  | Var x -> 
      begin match List.assoc_opt x ctx with
        | None -> raise FreeVariable
        | Some a -> a
      end
               
  | Primop (op, es) -> infer_op op (List.map (infer ctx) es)
                         
  | If (cond, e1, e2) -> 
      begin match infer ctx cond with 
        | Bool -> 
            begin match (infer ctx e1),(infer ctx e2) with
              | t1,t2 when t1 = t2 -> t1
              | _ -> raise TypeMismatch
            end
        | _ -> raise TypeMismatch
      end
                           
  | Let (x, e1, e2) -> 
      let t = infer ctx e1 in
      infer ((x,t) :: ctx) e2
                         
  | Fn (xs, e') -> 
      let t2 = infer (xs @ ctx) e' in
      Arrow (get_type xs,t2)
                     
  | Apply (e', args) -> 
      begin match infer ctx e' with
        | Arrow (t, t') -> 
            if (List.length t) <> (List.length args) then 
              raise ArityMismatch 
            else
              let t_args = List.map (infer ctx) args in
              if t_args = t then t'
              else raise TypeMismatch
        | _ -> raise TypeMismatch
      end
                          
  | Rec (f, t, e') -> 
      let t2 = infer ((f,t) :: ctx) e' in
      if t2  = t then t
      else
        raise TypeMismatch