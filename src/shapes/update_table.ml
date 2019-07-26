open Bedrock
open Paperwork
open Util
open Error
module D = Timetable.Day

type t = (string, D.t) Hashtbl.t

let empty () = Hashtbl.create 1

let store_update table = function
  | Qexp.(Node [ String (_, name); Keyword timecode ]) ->
    let open Result in
    let* code = D.from_string timecode in
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
    let table = empty () in
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
      node [ string key; keyword $ D.to_string value ] :: acc)
    table
    []
  |> Qexp.node
;;

let fetch table key = Hashtbl.find_opt table key

let push table key value =
  let () =
    match fetch table key with
    | None ->
      Hashtbl.add table key value
    | Some x ->
      if D.cmp x value < 0
      then (
        let () = Hashtbl.remove table key in
        Hashtbl.add table key x)
  in
  table
;;
