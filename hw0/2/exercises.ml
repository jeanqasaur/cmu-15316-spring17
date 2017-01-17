open OUnit2;;

exception Unimplemented of string;

(* Fibonacci *)
let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> raise (Unimplemented "fib")

(* Fibonacci tests. *)
let test_fib n r = assert_equal (fib n) r
let test_all_fib test_ctxt =
  List.fold_left (fun () (n, r) -> test_fib n r)
    () [(0, 0); (1, 1); (2, 1); (3, 2); (4, 3); (5, 5); (6, 8)]

(* Defining the test suite. *)
let suite =
  "suite">:::
    ["test_fib">:: test_all_fib]

(* Run the test suite. *)
let () =
  run_test_tt_main suite
