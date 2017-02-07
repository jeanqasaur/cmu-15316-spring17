
open Ast

(* Response codes *)
type response = 
    ResponseTimeout
  | ResponseFailed
  | ResponseDenied 
  | ResponseCreatePrincipal (* CREATE_PRINCIPAL *)
  | ResponseChangePassword (* CHANGE_PASSWORD *)
  | ResponseSet (* SET *)
  | ResponseAppend (* APPEND *)  
  | ResponseForEach (* FOREACH *)
  | ResponseFilterEach (* FILTEREACH *)
  | ResponseReturning of var_value (* "RETURNING "^(string_of_var_value v) *)
  | ResponseExiting (* EXITING *)
  | ResponseSetDelegation (* SET_DELEGATION *)
  | ResponseDeleteDelegation (* DELETE_DELEGATION *)  
;;

(* Failure modes *)
exception TimeoutException;;
exception Denied;;
exception Failed;;
exception ResourceExhaustion;;

(* Helper function to JSON-encode server responses *)
module ResponseToJSON = Aeson.MakeT( struct
  type t = response
  let toJSON r = 
    let o s = [("status", `String s)] in
    `Assoc ( match r with
      ResponseTimeout -> o "TIMEOUT"
    | ResponseFailed -> o "FAILED"
    | ResponseDenied -> o "DENIED"
    | ResponseCreatePrincipal -> o "CREATE_PRINCIPAL"
    | ResponseChangePassword -> o "CHANGE_PASSWORD"
    | ResponseSet -> o "SET"
    | ResponseAppend -> o "APPEND"    
    | ResponseForEach -> o "FOREACH"
    | ResponseFilterEach -> o "FILTEREACH"
    | ResponseExiting -> o "EXITING"
    | ResponseReturning v -> ("output", var_value_to_json v)::(o "RETURNING")
    | ResponseSetDelegation -> o "SET_DELEGATION"
    | ResponseDeleteDelegation -> o "DELETE_DELEGATION"    
    )
  
end )
;;

let setup pass =
  (* Perform server initialization *)
  raise Failed
;;

(* Evaluate expressions as per the spec *)
let rec eval_expr (e:expr) : var_value =
  (* Printf.printf "eval |%s|\n" (string_of_expr e); flush stdout; *)
  match e with
    | Val_expr se -> raise Failed
    | Empty_list -> raise Failed
    | Rec_exp fes -> raise Failed
    | If (b,e1,e2) -> raise Failed

(* Evaluate Boolean expressions as per the spec *)
let rec eval_bexpr (b:bool_expr) : bool =
  match b with
      True -> raise Failed
    | False -> raise Failed
    | Equal(args) -> raise Failed
    | Notequal(args) -> raise Failed
;;

(* Evaluate commands as per the spec *)
let rec eval_cmd (cmd:cmd) : response * (var_value option) =
  (* Printf.eprintf "eval |%s|; store size is |%d|, sec-state size is |%d|\n" (string_of_cmd cmd) (State.size state) (State.size Auth.sec); flush stdout;  *)  
  match cmd with
    | Create_principal (p,s) -> raise Failed
    | Change_pass (p,s) -> raise Failed
    | Set (x,e) -> raise Failed
    | Append (x,e) -> raise Failed
    | Filtereach (y,x,b) -> raise Failed
    | Return e -> raise Failed
    | Exit -> raise Failed
;;

(* Evaluate a top-level program *)
let eval ((prin,pass,cmds):prog) : (response list) * var_value =
  raise Failed
;;

(* Execute function f, timing out after t seconds *)
let timeout f (t:float) = 
  try
    let _ = Unix.setitimer Unix.ITIMER_REAL {Unix.it_value = t; Unix.it_interval = 0.0} in
    let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeoutException)) in
    let res = f () in
    let _ = Unix.setitimer Unix.ITIMER_REAL {Unix.it_value = 0.0; Unix.it_interval = 0.0} in
    Some res
  with 
      TimeoutException -> 
        None
    | e -> 
      let _ = Unix.setitimer Unix.ITIMER_REAL {Unix.it_value = 0.0; Unix.it_interval = 0.0} in
      raise e  
;;

let run inc =
  let prog_str = raise Failed in (* Read the program from incoming buffer inc, as per the spec *)
  match prog_str with
    | None -> [ResponseTimeout]
    | Some (s) -> 
      let lexbuf = Lexing.from_string s in
      try
        let p = Parser.prog Lexer.token lexbuf in
        let (status,_) = eval p in
        status
      with
      | Failed -> [ResponseFailed]
      | Denied -> [ResponseDenied]
      | ResourceExhaustion ->
        Printf.eprintf "Got resource exhaustion\n"; flush stderr;
        [ResponseTimeout]
      | Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.eprintf "Line %d, character %d: syntax error at %s\n%!"
                line cnum
                ( if tok = "\n" 
                  then "newline" 
                  else Printf.sprintf "`%s'" tok );
        [ResponseFailed]
      | exn ->
        Printf.eprintf "Got exception %s\n" (Printexc.to_string exn);
        [ResponseFailed]
;;
