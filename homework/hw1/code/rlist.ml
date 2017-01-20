type 'a rlist = bool * 'a list
(* bool = true means the list is left-to-right; false means right to left *)

let from_list l = (true,l)

let to_list (dir,l) =
  match dir with
      true -> l
    | false -> List.rev l
  
let map f (dir,l) =
  let rec map_aux acc = function
    | [] -> acc
    | x :: xs -> map_aux (f x :: acc) xs
  in match dir with
      true -> (false,map_aux [] l)
    | false -> (false,map_aux [] (List.rev l))

let append l1 l2 =
  match l1,l2 with
    | (false,l1'),(true,l2') -> (true,List.rev_append l1' l2')
    | (false,l1'),(false,l2') -> (true,List.rev_append l1' (List.rev l2'))
    | (true,l1'),(true,l2') -> (true,List.rev_append (List.rev l1') l2')
    | (true,l1'),(false,l2') -> (false,List.rev_append (List.rev l2') (List.rev l1'))

let filter p (dir,l) =
  let rec find accu = function
  | [] -> accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  match dir with
      true -> (false,find [] l)
    | false -> (false, find [] (List.rev l))
;;

let fold_left f acc (_,l) = List.fold_left f acc l
