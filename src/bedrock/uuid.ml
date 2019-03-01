open Util

type t = string

let general_clock = Clock.int ()

module type GENERATOR = sig
  val name : unit -> string
  val pid : unit -> int
  val time : unit -> float
end

let dashify hashed_str =
  Scanf.sscanf
    hashed_str
    "%8s%4s%4s%4s%12s"
    (Printf.sprintf "%s-%s-%s-%s-%s")
;;

let _make clock hname pid time =
  let p = string_of_int pid in
  let t = string_of_float time in
  let i = string_of_int (Clock.next clock) in
  String.concat "-" [hname; p; t; i] |> md5 |> dashify
;;

let make (module G : GENERATOR) () =
  _make general_clock $ G.name () $ G.pid () $ G.time ()
;;

module Generator (G : GENERATOR) = struct
  type t = string

  let clock = Clock.int ()
  let make () = _make clock $ G.name () $ G.pid () $ G.time ()
  let to_string x = x
end
