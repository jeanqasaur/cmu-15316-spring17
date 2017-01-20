(* Command-line debugging interface for core server functionality *)

let _ =
  let go inc =
    let s = 
      try 
        Interp.run inc
      with e ->
        Printf.eprintf "Got unexpected exception %s\n" (Printexc.to_string e);
        [Interp.ResponseFailed]
    in
      List.iter 
        (fun r ->
          let _ = print_string @@ Interp.ResponseToJSON.encode r in
          let _ = print_string "\n" in
          ()) s
  in
  (* input file either via stdin, or given as first arg on command line *)
  let argn = Array.length Sys.argv in
  let inc =
    if  argn > 1 then
      open_in Sys.argv.(1)
    else
      stdin in
  (* set up initial server state *)
  let _ = Interp.setup "admin" in
  (* run the prorgram, print its output *)
  go inc;
  (* run additional programs, if given *)
  if argn > 2 then 
    for i = 2 to (argn-1) do
      let inc = open_in Sys.argv.(i) in
      go inc;
      close_in inc
    done;
    exit 0  
;;
