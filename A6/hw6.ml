(*--------------------------------------------------------------*)
(* Q1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  let tab = tabulate (fun x -> x + 1) (String.length s) in
  let get n = String.get s (n - 1) in
  List.map get tab

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  List.fold_right (^) (List.map Char.escaped l) ""

(* ------------------------------------------------------------------------*)
(* Q2 : Bank Account *)
(* ------------------------------------------------------------------------*)

let open_account (pass: password) : bank_account =
  let current = ref pass in 
  let balance = ref 0 in
  let wrong = ref 0 in
  {
    update_pass = 
      (fun oldPass newPass -> let state = not ((!wrong) = 3) in
        let _ = if not (String.equal oldPass (!current)) && state then (wrong := !wrong + 1) else 
          if (String.equal oldPass !current) && state then (wrong:= 0) else () in 
        if not state then raise account_locked else
        if not (String.equal oldPass (!current)) then raise wrong_pass else
          current := newPass); 
    retrieve = 
      (fun pass num -> let state = not ((!wrong) = 3) in
        let _ = if not (String.equal pass (!current)) && state then (wrong := !wrong + 1) else 
          if (String.equal pass !current) && state then (wrong:= 0) else () in
        if not state then raise account_locked else
        if not (String.equal pass (!current)) then raise wrong_pass else 
          match num with 
          | n when n < 0 -> raise negative_amount
          | n -> if n > !balance then raise not_enough_balance 
              else balance := !balance - n);
    deposit = 
      (fun pass num -> let state = not ((!wrong) = 3) in
        let _ = if not (String.equal pass (!current)) && state then (wrong := !wrong + 1) else 
          if (String.equal pass !current) && state then (wrong:= 0) else () in
        if not state then raise account_locked else
        if not (String.equal pass (!current)) then raise wrong_pass else 
        if num < 0 then raise negative_amount else
          balance := !balance + num);
    show_balance = 
      (fun pass -> let state = not ((!wrong) = 3) in
        let _ = if not (String.equal pass (!current)) && state then (wrong := !wrong + 1) else 
          if (String.equal pass !current) && state then (wrong:= 0) else () in
        if not state then raise account_locked else
        if not (String.equal pass (!current)) then raise wrong_pass else 
          !balance);
  }