
open Yojson.Basic ;;

module type ToJSON = sig
  type t
  val toJSON : t -> json
end

module type T = sig
  type t
  val encode : t -> string
  val encodeToChannel : out_channel -> t -> unit
end

module MakeT (M : ToJSON) : T with type t = M.t = struct
  type t = M.t
  let encode x = to_string @@ M.toJSON x
  let encodeToChannel c x = to_channel c @@ M.toJSON x
end

