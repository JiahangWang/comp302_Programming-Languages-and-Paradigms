(**To DO: Write a good set of tests for free_variables **)
let free_variables_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Let, Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), []); 
  (ex1,[]); 
  (Rec ("x",Int,Var "x"),[]);
  (Apply (Var "f", [I 3; I 4]),["f"]);
  (ex7,[])
]

(* TODO: Implement the missing cases of free_variables. *)
let rec free_variables : exp -> name list =
  let rec getname l = 
    match l with 
    | [] -> []
    | (a,b)::xs -> a :: (getname xs) in
  (* Taking unions of lists.
     If the lists are in fact sets (all elements are unique),
     then the result will also be a set.
  *)
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (_, args) -> union_fvs args
  | Fn (xs, e) ->
      delete  (getname xs) (free_variables e)
  | Rec (x, _, e) ->
      delete [x] (free_variables e)
  | Let (x, e1, e2) ->
      union (free_variables e1) (delete [x] (free_variables e2))
  | Apply (e, es) -> 
      free_variables e


(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  
  (((I 1, "x"), (* 5 *)
    ex1), 
   ex1);
  
  (((Primop (Plus, [Var "a"; Var "b"]), "x"), (* 6 *)
    Fn ([("a", Int); ("b", Int)],Primop (Times, [Var "x"; Var "b"]))), 
   Fn ([("a4", Int); ("b3", Int)],
       Primop (Times, [Primop (Plus, [Var "a"; Var "b"]); Var "b3"])));
  
  (((Primop (Plus, [I 2; Var "f"]), "x"), (* 3 *)
    Rec ("f", Arrow ([Int], Int), Primop (Negate, [Var "x"]))), 
   Rec ("f4", Arrow ([Int], Int),
        Primop (Negate, [Primop (Plus, [I 2; Var "f"])])));
  
  (((I 1, "x"), (* 1 *)
    Rec ("f", Arrow ([Int], Int), Primop (Negate, [Var "x"]))), 
   Rec ("f", Arrow ([Int], Int), Primop (Negate, [I 1]))); 
  
  (((B true, "y"), (* 4 *)
    Fn ([("x", Bool)], If (Var "y", I 1, I 0))), 
   Fn ([("x", Bool)], If (B true, I 1, I 0)));
  
   
  (((I 1, "g"), (* 7 *)
    Apply (Var "g", [Var "x"; I 2])), 
   Apply (I 1, [Var "x"; I 2]));
  
  (((I 1, "fact"), (* 2 *)
    Rec ("fact", Arrow ([Int], Int), Fn (["n", Int],
                                         If (Primop (Equals, [Var "n"; I 0]), I 1,
                                             Primop (Times, [Var "n"; Apply (Var "fact", [Primop (Minus, [Var "n"; I 1])])]))))), 
   Rec ("fact1", Arrow ([Int], Int),
        Fn ([("n", Int)],
            If (Primop (Equals, [Var "n"; I 0]), I 1,
                Primop (Times,
                        [Var "n"; Apply (Var "fact1", [Primop (Minus, [Var "n"; I 1])])])))));
  
  (((Primop (Plus, [I 2; Var "f"]), "x"), (* 8 *)
    Apply (ex1, [Var "f"; Var "x"])), 
   Apply
     (Fn ([("x", Int); ("y", Int)],
          Primop (Plus,
                  [Primop (Times, [Var "x"; Var "x"]); Primop (Times, [Var "y"; Var "y"])])),
      [Var "f"; Primop (Plus, [I 2; Var "f"])]));
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  (*helper*)
  let rec getname l = 
    match l with 
    | [] -> []
    | (a,b)::xs -> a :: (getname xs) in 
  (*helper*)
  let rec mer l1 l2 = match (l1,l2) with
    | ([],[]) -> []
    | ((a,b) :: xs,c :: cs) -> (c,b) :: mer xs cs
    | _ -> failwith "DNE" 
  in
  
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y = x then
        Rec (y, t, e) 
      else
        let (y, e) =
          if List.mem y (free_variables e') then
            rename y e
          else
            (y, e)
        in
        Rec (y,t,subst s e)

  | Fn (xs, e) -> 
      if List.mem x (getname xs) then
        Fn (xs,e)
      else 
        let diff = delete (free_variables e') (getname xs) in
        if diff = (getname xs) then
          Fn (xs,subst s e) 
        else
          let (p,exp) = rename_all (getname xs) e in
          Fn ((mer xs p),subst s exp)

  | Apply (e, es) -> 
      let rec listsub ss = match ss with
        | [] -> []
        | x :: xs -> (subst s x) :: listsub xs
      in
      Apply(subst s e, listsub es)

          
and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs 