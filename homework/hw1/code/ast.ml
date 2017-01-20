(* Syntax definition and helper functions *)

type ident = string
type principal =
    Admin
  | Anyone
  | Principal of ident
type value =
    String of string
  | Rec of (ident * string) list
type var_value =
    Value of value
  | Table of value Rlist.rlist
type small_expr =
    Var of ident
  | Field of ident * ident
  | String_exp of string
type bool_expr =
    True
  | False
  | Equal of small_expr list
  | Notequal of small_expr list  
type expr =
    Val_expr of small_expr
  | Empty_list
  | Rec_exp of (ident * small_expr) list  
  | If of bool_expr * expr * expr
type right =
    Read
  | Write
  | Append  
type cmd =
    Create_principal of principal * string
  | Change_pass of principal * string
  | Set of ident * expr
  | Append of ident * expr
  | Filtereach of ident * ident * bool_expr  
  | Return of expr
  | Exit
type prog =
    principal * string * (cmd list)

(* Helper: tail-recursive map *)

let tr_map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l 

let tr_append l1 l2 =
  let l1' = List.rev l1 in
  List.rev_append l1' l2
    
(* Pretty printing *)
      
let string_of_list printer l lb rb =
  let rec aux () = function
    | [] -> ""
    | [h] -> Printf.sprintf "%s" (printer h)
    | h::t -> Printf.sprintf "%s,%a" (printer h) aux t
  in Printf.sprintf "%s%a%s" lb aux l rb
  
let rec string_of_value v =
  let string_of_field (f,s) = f ^"=\""^ s ^ "\"" in
  match v with
      String s -> "\"" ^ s ^ "\""
    | Rec fs -> string_of_list string_of_field (List.sort compare fs) "{" "}"

let string_of_var_value v =
  match v with
      Value v -> string_of_value v
    | Table vs -> string_of_list string_of_value (Rlist.to_list vs) "[" "]"

let string_of_right r =
  match r with
      Read -> "read"
    | Write -> "write"
    | Append -> "append"    

let string_of_small_expr se =
  match se with
      Var x -> x
    | Field (x,y) -> x ^ "." ^ y
    | String_exp s -> "\"" ^ s ^ "\""

let string_of_boolexpr b =
  match b with
      True -> "true"
    | False -> "false"
    | Equal(args) -> "equal("^(string_of_list string_of_small_expr args "(" ")")
    | Notequal(args) -> "notequal("^(string_of_list string_of_small_expr args "(" ")")

let string_of_principal p =
  match p with
      Admin -> "admin"
    | Anyone -> "anyone"
    | Principal x -> x
      
let rec string_of_expr e =
  let string_of_field (f,se) = f ^"=\""^ (string_of_small_expr se) ^ "\"" in
  match e with
      Val_expr se -> string_of_small_expr se
    | Empty_list -> "[]"
    | Rec_exp fes -> string_of_list string_of_field fes "{" "}"    
    | If (b,e1,e2) -> "if "^(string_of_boolexpr b)^" then "^(string_of_expr e1)^" else "^(string_of_expr e2)
      
let string_of_cmd c =
  match c with
      Create_principal (p,s) -> "create principal " ^ (string_of_principal p) ^ " \"" ^ s ^ "\""
    | Change_pass (p,s) -> "change password "^ (string_of_principal p) ^ " \"" ^ s ^ "\""
    | Set (x,e) -> "set "^x^" = " ^ (string_of_expr e)
    | Append (x,e) -> "append "^(string_of_expr e)^" to "^x
    | Filtereach (y,x,e) -> "filtereach "^y^" in "^x^" with "^(string_of_boolexpr e)    
    | Return e -> "return "^(string_of_expr e)
    | Exit -> "exit"

let string_of_prog (p,s,cmds) =
  let rec string_of_cmds cs s =
    match cs with
  [] -> s
      | cmd::t ->
  let s' = string_of_cmd cmd in
  string_of_cmds t (s^"\n"^s') in
  string_of_cmds cmds ("as principal "^(string_of_principal p)^" password \""^s^"\" do")

(* Conversion to JSON *)

let value_to_json v : Yojson.Basic.json = match v with
    String s -> `String s
  | Rec fs -> `Assoc (tr_map (fun (k,v) -> (k, `String v)) fs) 

let var_value_to_json v : Yojson.Basic.json = match v with
    Value v -> value_to_json v
  | Table vs -> `List (tr_map value_to_json (Rlist.to_list vs)) 

(* Sizes *)

let size_of_principal = function
  | Admin -> 1
  | Anyone -> 1
  | Principal s -> 1 + String.length s
    
let size_of_var_value vv =
  let size_of_value v =
    match v with
  String s -> String.length s
      | Rec fs -> List.fold_left (fun acc (f,s) -> 1 + acc + (String.length f) + (String.length s)) 0 fs in
  match vv with
    | Value v -> size_of_value v
    | Table vs -> Rlist.fold_left (fun acc v -> 1 + acc + size_of_value v) 0 vs
