open Ast

(*
 * Below are basic definitions for 
 * setting up the security automaton's state. 
 * You are not required to use these definitions,
 * and can use other containers, or add additional
 * definitions, as you deem appropriate.
 *)
(* container for the set of username, password pairs *)
module Credential = struct
  type t = principal * string
  let compare = compare
end
module Credentials = Set.Make(Credential)

(* container for the set of variables *)
module Variable = struct
  type t = ident
  let compare = compare
end
module Variables = Set.Make(Variable)

(* container for the set of permissions *)
module Permission = struct
  type t = principal * ident * right
  let compare = compare
end
module Permissions = Set.Make(Permission)

(* 
 * Type for the security automaton state.
 * Note that all of the containers are mutable
 * in this type, as Set is not itself mutable.
 *)
type refmon_state =
  {    
    mutable p: Credentials.t;
    mutable c: principal;
    mutable v: Variables.t;
    mutable a: Permissions.t
  }

(* type for the automaton input symbols, as described in the handout *)
type input_symbol =
    SetCurrentPrin of principal * string
  | Oper of ident * right
  | AddRight of principal * ident * right
  | RemoveRight of principal * ident * right
  | AddPrincipal of principal * string
  | UpdatePrincipal of principal * string
  | AddVariable of ident
  | Terminate

(* 
 * Nullary function that returns an initial security automaton
 * state. This does not add Admin to the initial set of users,
 * so the state returned here will need to be updated with
 * Admin's password before the automaton is fully-initialized.
 *)
let init_refmon =  
  {
    p = Credentials.add (Anyone, "") Credentials.empty;
    c = Anyone;
    v = Variables.empty;
    a = Permissions.empty
  }

exception PrincipalNotExists;;
exception PrincipalExists;;

(* 
 * Helper function to add a new username/password pair to 
 * the credentials.
 * 
 * Throws PrincipalExists if principal p is already in the state.
 *)
let add_credential (state: refmon_state) (p: principal) (pw: string) =
  if (Credentials.exists (fun (el: principal * string) -> let (cp, _) = el in cp = p) state.p) then 
    raise PrincipalExists;
  state.p <- Credentials.add (p,pw) state.p

(* 
 * Helper function to check password credentials for a user.
 * 
 * Throws PrincipalNotExists if principal p is not already in the state.
 *)
let check_password (state: refmon_state) (p: principal) (pw: string) = 
  if (not (Credentials.exists (fun (el: principal * string) -> let (cp, _) = el in cp = p) state.p)) then 
    raise PrincipalNotExists;  
  Credentials.mem (p,pw) state.p