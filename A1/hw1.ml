(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
  (((1,2),(1,2)),0);
  (((3,4),(5,6)),4);
  (((5,6),(3,4)),4);
  (((-5,-6),(-3,-4)),4);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance (x1, y1) (x2, y2) = 
  let abs x y = if (x - y) >= 0 then x - y else
      y - x
  in (abs x1 x2) + (abs y1 y2)



(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((3, 2), 3);
  ((0, 0), 1);
  ((1, 0), 1);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial n k =
  let rec factorial x = 
    if x = 0 then 1 else x * factorial(x - 1)
  in
  (factorial n) / ((factorial k) * (factorial (n - k))) 



(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  (0,2);
  (1,1);
]

(* TODO: Implement a tail-recursive helper lucas_helper. *)
let rec lucas_helper n s f =
  if n = 0 then s else
  if n = 1 then f else
    lucas_helper (n - 1) f (s + f)


(* TODO: Implement lucas that calls the previous function. *)
let lucas n =
  lucas_helper n 2 1
