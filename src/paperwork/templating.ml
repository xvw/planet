open Bedrock

type ruleset = (string, string option -> string Result.t) Hashtbl.t

let ruleset rules =
  let seq = List.to_seq rules in
  Hashtbl.of_seq seq
;;

let apply _ruleset _content = Ok ""
