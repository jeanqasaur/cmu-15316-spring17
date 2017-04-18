open Ast;;

(* principal and string_of_principal are imported from ast.ml *)
(*
type principal =
  | Admin
  | Anyone
  | Principal of string;;

let string_of_principal = function
  | Admin -> "admin"
  | Anyone -> "anyone"
  | Principal s -> s;;
*)

type action = string;; (* Change this *)

let string_of_action = function
  | s -> s;; (* Change this *)

type stmt =
  | Action of action
  | Says of principal * stmt
  | Speaksfor of principal * principal
  | Delegates of principal * principal * action
  | Impl of stmt * stmt;;

let rec string_of_stmt = function
  | Action act -> string_of_action act
  | Says (p,stm) -> string_of_principal p ^ " says (" ^ string_of_stmt stm ^ ")"
  | Speaksfor (p1,p2) -> string_of_principal p1 ^ " speaksfor " ^ string_of_principal p2
  | Delegates (p1,p2,act) -> string_of_principal p1 ^ " delegates " ^ string_of_action act
                             ^ " to " ^ string_of_principal p2
  | Impl (stm1,stm2) -> "(" ^ string_of_stmt stm1 ^ ") implies (" ^ string_of_stmt stm2;;

type proof =
  | SaysI2 of proof * stmt
  | SaysImpl of proof * proof * stmt
  | SpeaksE1 of proof * proof * stmt
  | DelegateE of proof * proof * stmt
  | Given of int * stmt;;

let string_of_proof proof =
  let string_of_rule = function
    | SaysI2 (_,stm) -> "(Says-I2) " ^ string_of_stmt stm
    | SaysImpl (_,_,stm) -> "(Says-Impl) " ^ string_of_stmt stm
    | SpeaksE1 (_,_,stm) -> "(Speaks-E1) " ^ string_of_stmt stm
    | DelegateE (_,_,stm) -> "(Delegate-E) " ^ string_of_stmt stm
    | Given (line,stm) -> "(" ^ string_of_int line ^ ") " ^ string_of_stmt stm in
  let rec string_of_proof' proof lvl =
    String.make (2*lvl) ' ' (*indent*) ^ string_of_rule proof ^ "\n" ^
    match proof with
    | SaysI2 (pf,_) -> string_of_proof' pf (lvl+1)
    | SaysImpl (pf1,pf2,_) -> string_of_proof' pf1 (lvl+1) ^ string_of_proof' pf2 (lvl+1)
    | SpeaksE1 (pf1,pf2,_) -> string_of_proof' pf1 (lvl+1) ^ string_of_proof' pf2 (lvl+1)
    | DelegateE (pf1,pf2,_) -> string_of_proof' pf1 (lvl+1) ^ string_of_proof' pf2 (lvl+1)
    | Given _ -> "" in
string_of_proof' proof 0;;
