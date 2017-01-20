(* 
    Main server entry point.
    Listens for TCP connections on the specified port,
    and sends incoming programs to the interpreter.
*)

let main_server serv_fun =
  let _ = Sys.signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0)) in
  let toolong b a = b || (String.length a > 4096) in
  if Array.length Sys.argv < 2
    || Array.length Sys.argv > 3
    || Array.fold_left toolong false Sys.argv
  then (Printf.eprintf "usage : %s port [password]\n" Sys.argv.(0); exit 255)
  else
    let port = int_of_string Sys.argv.(1) in
    let pass =
      if Array.length Sys.argv > 2 then Sys.argv.(2) else "admin" in
    let my_address = Unix.inet_addr_any in  
    let sockaddr = Unix.ADDR_INET(my_address, port) in
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0  in
    Unix.bind sock sockaddr ;
    Unix.listen sock 3;
    let _ = Interp.setup pass in
    while true do
      let (s, caller) = Unix.accept sock in
      let inchan = Unix.in_channel_of_descr s
      and outchan = Unix.out_channel_of_descr s
      in serv_fun inchan outchan ;
      Unix.close s
    done;
    Unix.close sock
;;

let interp_service ic oc =
  let res = 
    try 
      Interp.run ic
    with e ->
      Printf.eprintf "Got unexpected exception %s\n" (Printexc.to_string e);
      [Interp.ResponseFailed]
  in  
  List.iter (fun r -> 
      Interp.ResponseToJSON.encodeToChannel oc r;      
      output_string oc "\n";
    ) res;
  flush oc
;;

let go_interp_service () =
  try
    main_server interp_service;
    exit 0
  with Unix.Unix_error (e,f,arg) ->
    (Printf.eprintf "\"%s\" failed: %s\n" f (Unix.error_message e);
     exit 63) 
;;

go_interp_service();;
