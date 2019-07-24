open Bedrock
open Paperwork
open Util
open Error

type t = (string, Timetable.Day.t) Hashtbl.t

let store_update table = function
  | Qexp.(Node [ String (_, name); Keyword timecode ]) ->
    let open Result in
    let* code = Timetable.Day.from_string timecode in
    let+ final_table =
      let () = Hashtbl.remove table name in
      let () = Hashtbl.add table name code in
      return table
    in
    final_table
  | qexp ->
    Error (Not_a_valid_node (Qexp.to_string qexp))
;;

let from_qexp = function
  | Qexp.Node elts ->
    let table = Hashtbl.create 1 in
    List.fold_left
      (fun potential_table expr ->
        let open Result.Syntax in
        let* hashtable = potential_table in
        store_update hashtable expr)
      (Ok table)
      elts
  | qexp ->
    Error (No_root_element (Qexp.to_string qexp))
;;

let to_qexp table =
  Hashtbl.fold
    (fun key value acc ->
      let open Qexp in
      node [ string key; keyword $ Timetable.Day.to_string value ]
      :: acc)
    table
    []
  |> Qexp.node
;;
