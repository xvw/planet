open Bedrock
open Error

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let from_qexp f qexp =
  match qexp with
  | Qexp.Node elts ->
    let table = Hashtbl.create 1 in
    let rec aux = function
      | Error x, _ ->
        Error x
      | table, [] ->
        table
      | Ok table, Qexp.Node (key :: value) :: xs ->
        let open Result.Infix in
        let new_table =
          f key value
          >>= fun (k, v) ->
          let () = Hashtbl.add table k v in
          Ok table
        in
        aux (new_table, xs)
      | _, exp :: _ ->
        Error (Not_a_valid_node (Qexp.to_string exp))
    in
    aux (Ok table, elts)
  | _ ->
    Error (No_root_element (Qexp.to_string qexp))
;;
