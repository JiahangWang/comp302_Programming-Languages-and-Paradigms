let xyz : Variable_set.t
  =  Variable_set.singleton "x"
     |> Variable_set.add "y"
     |> Variable_set.add "z"
       
let xyz_truth_asgn : truth_assignment
  =  Variable_map.singleton "x" true
     |> Variable_map.add "y" false
     |> Variable_map.add "z" false 
(** Question 1 *)

(* TODO: Add test cases. *)
let collect_variables_tests : (formula * Variable_set.t) list = [
  ((parse_formula "(x&y)|~z"),xyz);
  ((parse_formula "x|~(y&~z)"),xyz)
]

(* TODO: Implement the function. *)
let rec collect_variables (formula : formula) : Variable_set.t = 
  let set = Variable_set.empty in
  match formula with 
  | Variable x -> Variable_set.add x set
  | Conjunction (x,y) -> 
      Variable_set.union (collect_variables x) (collect_variables y)
  | Disjunction (x,y) ->
      Variable_set.union (collect_variables x) (collect_variables y)
  | Negation x -> collect_variables x
      

(** Question 2 *)

(* TODO: Add test cases. *)
let eval_success_tests : ((truth_assignment * formula) * bool) list = [
  ((xyz_truth_asgn,parse_formula "(x&y)|~z"),true); 
]

(* TODO: Add test cases. *)
let eval_failure_tests : ((truth_assignment * formula) * exn) list = [ 
  ((xyz_truth_asgn,parse_formula "(x&y)|~m"),Unassigned_variable "m");
  ((xyz_truth_asgn,parse_formula "~(x|z)|(y&~m)"),Unassigned_variable "m");
]

(* TODO: Implement the function. *)
let rec eval (state : truth_assignment) (formula : formula) : bool =
  match formula with 
  | Variable x -> 
      let value = Variable_map.find_opt x state in 
      (match value with
       | Some n -> n
       | _ -> raise (Unassigned_variable x))
  | Conjunction (x,y) -> 
      let l = eval state x in let r = eval state y in l && r
  | Disjunction (x,y) ->
      let l = eval state x in let r = eval state y in l || r
  | Negation x ->
      not(eval state x)
               
(** Question 3 *)

(* TODO: Add test cases. *)
let find_satisfying_assignment_tests : (formula * truth_assignment option) list = [
  (parse_formula "~((x&y)|z)",Some(Variable_map.singleton "x" true
                                   |> Variable_map.add "y" false
                                   |> Variable_map.add "z" false));
  (parse_formula "x&~x", None)
]

(* TODO: Implement the function. *)
let find_satisfying_assignment (formula : formula) : truth_assignment = 
  let elements = Variable_set.elements (collect_variables formula) in
  let truth = Variable_map.empty in
  let rec find_helper (formula) (elements) (truth) = 
    match elements with 
    | [] -> 
        if eval truth formula = true then truth
        else raise Unsatisfiable_formula
    | x :: xs ->
        try find_helper (formula) (xs) (Variable_map.add x true truth)
        with
        | Unsatisfiable_formula -> 
            find_helper (formula) (xs) (Variable_map.add x false truth)
  in find_helper (formula) (elements) (truth)