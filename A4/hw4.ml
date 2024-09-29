(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for tree depth. *)
let tree_depth_cps_tests : (int tree * int) list =
  [
    (Empty,0);
    (Tree(Tree(Tree(Empty,2,Empty),7,Tree(Tree(Empty,5,Empty),6,Tree(Empty,11,Empty))),
          1,
          Tree(Empty,9,Tree(Tree(Empty,5,Empty),9,Empty))),4)
  ]

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth t =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let tree_depth_cps (t: 'a tree) = 
  let rec helper (t: 'a tree) (sc: (int -> int)) =
    match t with
    | Empty -> sc 0
    | Tree(l,num,r) -> helper l (fun i1 -> max (sc(i1 + 1)) (helper r (fun i2 -> sc(i2 + 1))) )
  in helper t (fun i -> i)

(* Question 2(a): Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_tests : (int tree * int list) list = [
  (Empty,[]); 
  (Tree(Tree(Tree(Empty,2,Empty),7,Tree(Tree(Empty,5,Empty),6,Tree(Empty,11,Empty))),
        1,
        Tree(Empty,9,Tree(Tree(Empty,5,Empty),9,Empty))),[2; 5; 11; 6; 7; 5; 9; 9; 1])
]

(* TODO: Implement a CPS style postorder traversal function. *)
let traverse (tree : 'a tree) : 'a list = 
  let rec helper tree sc = match tree with
    | Empty -> sc ()
    | Tree(l,num,r) -> helper l (fun () -> helper r (fun () -> [num] @ (sc()) ) )
  in helper tree (fun () -> [])

(* Question 2(b): Distances from the Root *)
(* TODO: Write a good set of tests for testing the get_distances function. *)
let get_distances_tests : (int tree * int list) list = [
  (Empty,[]);
  (Tree(Tree(Tree(Empty,2,Empty),7,Tree(Tree(Empty,5,Empty),6,Tree(Empty,11,Empty))),
        1,
        Tree(Empty,9,Tree(Tree(Empty,5,Empty),9,Empty))),[10; 19; 25; 14; 8; 24; 19; 10; 1])
]

(* TODO: Implement a CPS style get_distances function. *)
let get_distances (tree : int tree) : int list = 
  let rec helper tree sum sc = match tree with
    | Empty -> sc ()
    | Tree(l,num,r) -> helper l (sum + num) (fun () -> helper r (sum + num) (fun () -> [sum + num] @ (sc())) )
  in helper tree 0 (fun () -> [])

(* Question 3: Finding Subtrees *)
(* TODO: Write a good set of tests for finding subtrees. *)
let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [
    (([1;2],Empty),None);
    (([1;7],Tree(Tree(Tree(Empty,2,Empty),7,Tree(Tree(Empty,5,Empty),6,Tree(Empty,11,Empty))),1,Tree(Empty,9,Tree(Tree(Empty,5,Empty),9,Empty))))
    ,Some (Tree (Empty, 2, Empty))); 
    (([],Tree(Tree(Tree(Empty,2,Empty),7,Tree(Tree(Empty,5,Empty),6,Tree(Empty,11,Empty))),1,Tree(Empty,9,Tree(Tree(Empty,5,Empty),9,Empty))))
    ,Some
       (Tree
          (Tree (Tree (Empty, 2, Empty), 7,
                 Tree (Tree (Empty, 5, Empty), 6, Tree (Empty, 11, Empty))),
           1, Tree (Empty, 9, Tree (Tree (Empty, 5, Empty), 9, Empty)))));
    (([2],Tree(Empty,1,Empty)),None);
    (([1;3;4],Tree(Empty,1,Tree(Empty,3,Tree(Empty,4,Empty)))),Some Empty)
  ]

(* TODO: Implement a CPS style find_subtree_cont function.*)
let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = 
    match ls with
    | [] -> sc tree
    | x :: xs -> 
        match tree with 
        | Tree(l,num,r) when x = num && xs = [] -> sc l
        | Tree(l,num,r) when num = x -> helper xs l sc (fun () -> helper xs r sc fc) 
        | _ -> fc ()
  in helper ls tree (fun x -> Some x) (fun () -> None) 