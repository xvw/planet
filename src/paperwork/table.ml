open Bedrock
open Util
open Error

type ('a, 'b) t = ('a, 'b) Hashtbl.t
type configuration = (string, Qexp.t option) t

let from_qexp_gen f qexp =
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

let from_qexp f =
  let rf x l =
    let open Qexp in
    match x with
    | Atom key | Tag key | Keyword key | String (_, key) ->
      f key l
    | q ->
      Error (Not_a_valid_node (to_string q))
  in
  from_qexp_gen rf
;;

let configuration =
  let open Qexp in
  let f key = function
    | [x] ->
      Ok (key, Some x)
    | [] ->
      Ok (key, None)
    | list ->
      Ok (key, Some (node list))
  in
  from_qexp f
;;

type ('a, 'b) fetchable =
  configuration -> string -> ('a -> 'b) -> 'b Validation.t

let fetch_string table field continuation =
  match Hashtbl.find_opt table field with
  | Some (Some (Qexp.String (_, k))) ->
    Ok (continuation k)
  | None ->
    Error [Undefined_field field]
  | _ ->
    Error [Invalid_field field]
;;

let fetch_string_opt table field continuation =
  match Hashtbl.find_opt table field with
  | Some (Some (Qexp.String (_, k))) ->
    Ok (continuation $ Some k)
  | None ->
    Ok (continuation None)
  | _ ->
    Error [Invalid_field field]
;;

let fetch_bool table field continuation =
  let open Qexp in
  match Hashtbl.find_opt table field with
  | None ->
    Error [Undefined_field field]
  | Some (Some x) ->
    (match x with
    | Keyword x | Atom x | Tag x | String (_, x) ->
      let b = String.lowercase_ascii x in
      if b = "true" || b = "false"
      then Ok (continuation (b = "true"))
      else Error [Invalid_field field]
    | _ ->
      Error [Invalid_field field])
  | _ ->
    Error [Invalid_field field]
;;

let fetch_bool_opt table field continuation =
  let open Qexp in
  match Hashtbl.find_opt table field with
  | None ->
    Ok (continuation None)
  | Some (Some x) ->
    (match x with
    | Keyword x | Atom x | Tag x | String (_, x) ->
      let b = String.lowercase_ascii x in
      if b = "true" || b = "false"
      then Ok (continuation $ Some (b = "true"))
      else Error [Invalid_field field]
    | _ ->
      Error [Invalid_field field])
  | _ ->
    Error [Invalid_field field]
;;
