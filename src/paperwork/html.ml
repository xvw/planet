open Bedrock
open Error

type attr =
  | Flag of string
  | Pair of string * string

type node =
  | Leaf of attr list
  | Node of (attr list * node list)

let process_attributes = function
  | Qexp.Node list | Qexp.Block list ->
    let open Result.Monad in
    List.fold_left
      (fun acc attributes ->
        acc
        >>= fun xs ->
        match attributes with
        | Qexp.(Tag flag | Atom flag | Keyword flag) ->
          return (Flag flag :: xs)
        | Qexp.(Node
                  [ (Tag key | Atom key | Keyword key)
                  ; ( Tag value
                    | Atom value
                    | Keyword value
                    | String (_, value) ) ]) ->
          return (Pair (key, value) :: xs)
        | qexp ->
          Error (InvalidAttribute (Qexp.to_string qexp)) )
      (return [])
      list
    >|= List.rev
  | qexp ->
    Error (InvalidAttribute (Qexp.to_string qexp))
;;
