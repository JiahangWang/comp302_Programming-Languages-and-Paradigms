(* SECTION 1 *) 

(* Question 1.1 *)
let rec take (n : int) (s : 'a stream) : 'a list =
  if n = 0 then [] 
  else s.head :: take (n - 1) (force s.tail)

(* Question 1.2 *)
let rec lucas1 = {
  head = 2;
  tail = Susp (fun () -> lucas2);
}
and lucas2 = {
  head = 1;
  tail = Susp (fun () -> add_streams lucas1 lucas2);
}

(* Question 1.3 *)
let rec unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  let helper x = let (_,r) = x in f r
  in
  str_map fst (iterate helper (f seed))

(* Question 1.4 *)
let lucas : int stream = 
  unfold (fun x -> let (l,r) = x in (l,(r, l + r))) (2,1)

(* SECTION 2 *)

(* Question 2.1 *)
let rec scale (s1 : int stream) (n : int) : int stream =
  str_map (fun x -> x * n) s1

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream = 
  if s1.head = s2.head then 
    {head = s1.head;
     tail = Susp (fun () -> merge (force s1.tail) (force s2.tail));
    }
  else if s1.head < s2.head then
    {head = s1.head;
     tail = Susp (fun () -> merge (force s1.tail) s2);
    }
  else
    {head = s2.head;
     tail = Susp (fun () -> merge s1 (force s2.tail));
    }
    

(* Question 2.2 *)
let rec s = {
  head = 1;
  tail = Susp (fun () -> merge s2 (merge s3 s5))
}
and s2 = {
  head = 2;
  tail = Susp (fun () ->  scale (force s.tail) 2)
}
and s3 = {
  head = 3;
  tail = Susp (fun () ->  scale (force s.tail) 3)
}
and s5 = {
  head = 5;
  tail = Susp (fun () ->  scale (force s.tail) 5)
} 