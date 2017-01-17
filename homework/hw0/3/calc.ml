exception Unimplemented of string;;

(* binop : ('a -> 'a -> 'a) -> 'a list -> 'a list *)
let binop op = function
  | b::a::r -> raise (Unimplemented "binop")
  | _ -> failwith "invalid expression"
 
(* interp : float list -> string -> string * float list *)
let interp s = function
  | "+" -> raise (Unimplemented "add")
  | "-" -> raise (Unimplemented "subtr")
  | "*" -> raise (Unimplemented "mult")
  | "/" -> raise (Unimplemented "div")
  | "^" -> raise (Unimplemented "exp")
  | str -> raise (Unimplemented "str")
 
(* interp_and_show : float list -> string -> float list *)
let interp_and_show s inp =
  let op,s' = interp s inp in
  Printf.printf "%s\t%s\t" inp op;
  List.(iter (Printf.printf "%F ") (rev s'));
  print_newline ();
  s'
 
(* rpn_eval : string -> float list *)
let rpn_eval str =
  Printf.printf "Token\tAction\tStack\n";
  let ss = Str.(split (regexp_string " ") str) in
  List.fold_left interp_and_show [] ss

let print_answer n =
  print_string ("***\n" ^ n ^ "\n");
  List.(iter (Printf.printf "%F\n") (rpn_eval n))

let _ =
  print_answer "3 2 5 + -";
  print_answer "3 2 + 5 -";
  print_answer "2 3 11 + 5 - *";
  print_answer "9 5 3 + 2 4 ^ - +"
