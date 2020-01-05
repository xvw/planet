open Bedrock
open Paperwork

let option obj k f =
  let open Option.Infix in
  obj >|= (fun x -> Qexp.kv k (f x)) |> Option.to_list
;;

let content = function
  | None -> []
  | Some (format, Text.File str) ->
    Qexp.
      [ node
          [ tag "content"
          ; string "external"
          ; keyword (Text.Format.to_string format)
          ; string str
          ]
      ]
  | Some (format, Text.Plain str) ->
    Qexp.
      [ node
          [ tag "content"
          ; string "internal"
          ; keyword (Text.Format.to_string format)
          ; string str
          ]
      ]
;;

let list key list f =
  let open Qexp in
  [ node [ tag key; node (List.map f list) ] ]
;;

let ziplist key list f =
  let open Qexp in
  [ node
      [ tag key
      ; node
          (List.map (fun (k, v) -> node [ string k; node (List.map f v) ]) list)
      ]
  ]
;;
