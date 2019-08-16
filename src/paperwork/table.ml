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
        let open Result.Syntax in
        let new_table =
          let* k, v = f key value in
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
    | [ x ] ->
      Ok (key, Some x)
    | [] ->
      Ok (key, None)
    | list ->
      Ok (key, Some (node list))
  in
  from_qexp f
;;

module Mapper = struct
  let string = function
    | Qexp.String (_, str) ->
      Ok str
    | q ->
      Error [ Mapping_failure ("string", Qexp.to_string q) ]
  ;;

  let token f = function
    | Qexp.String (_, str)
    | Qexp.Atom str
    | Qexp.Tag str
    | Qexp.Keyword str ->
      f str
    | q ->
      Error [ Mapping_failure ("token", Qexp.to_string q) ]
  ;;

  let couple f g = function
    | Qexp.Node [ x; y ] ->
      let open Validation.Applicative in
      (fun x y -> x, y) <$> f x <*> g y
    | q ->
      Error [ Mapping_failure ("couple", Qexp.to_string q) ]
  ;;

  let triple f g h = function
    | Qexp.Node [ x; y; z ] ->
      let open Validation.Applicative in
      (fun x y z -> x, y, z) <$> f x <*> g y <*> h z
    | q ->
      Error [ Mapping_failure ("triple", Qexp.to_string q) ]
  ;;
end

module Fetch = struct
  type 'a t = configuration -> string -> 'a Validation.t

  let option f table field =
    match f table field with
    | Ok x ->
      Ok (Some x)
    | Error [ Undefined_field _ ] ->
      Ok None
    | Error xs ->
      Error xs
  ;;

  let string table field =
    match Hashtbl.find_opt table field with
    | Some (Some (Qexp.String (_, k))) ->
      Ok k
    | None ->
      Error [ Undefined_field field ]
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let bool_refutable ?(default = true) table field =
    let open Qexp in
    match Hashtbl.find_opt table field with
    | None ->
      Ok default
    | Some (Some x) ->
      (match x with
      | Keyword x | Atom x | Tag x | String (_, x) ->
        let b = String.lowercase_ascii x in
        if b = "true" || b = "false"
        then Ok (b = "true")
        else Error [ Invalid_field field ]
      | _ ->
        Error [ Invalid_field field ])
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let bool table field =
    let open Qexp in
    match Hashtbl.find_opt table field with
    | None ->
      Error [ Undefined_field field ]
    | Some (Some x) ->
      (match x with
      | Keyword x | Atom x | Tag x | String (_, x) ->
        let b = String.lowercase_ascii x in
        if b = "true" || b = "false"
        then Ok (b = "true")
        else Error [ Invalid_field field ]
      | _ ->
        Error [ Invalid_field field ])
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let list mapper table field =
    let open Qexp in
    match Hashtbl.find_opt table field with
    | None ->
      Error [ Undefined_field field ]
    | Some (Some (Block elts)) | Some (Some (Node elts)) ->
      List.map mapper elts |> Validation.Applicative.sequence
    | Some (Some elt) ->
      List.map mapper [ elt ] |> Validation.Applicative.sequence
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let list_refutable mapper table field =
    let open Qexp in
    match Hashtbl.find_opt table field with
    | None ->
      Ok []
    | Some (Some (Block elts)) | Some (Some (Node elts)) ->
      List.map mapper elts |> Validation.Applicative.sequence
    | Some (Some elt) ->
      List.map mapper [ elt ] |> Validation.Applicative.sequence
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let aux_hashtbl f mapper table field =
    let open Validation.Infix in
    f
      Mapper.(couple $ token (fun x -> Ok x) $ fun x -> Ok x)
      table
      field
    >>= List.map (fun (x, y) -> mapper x y)
        %> Validation.Applicative.sequence
    >|= List.to_seq %> Hashtbl.of_seq
  ;;

  let hashtbl mapper table field =
    aux_hashtbl list mapper table field
  ;;

  let hashtbl_refutable mapper table field =
    aux_hashtbl list_refutable mapper table field
  ;;

  let aux_ziplist f mapper table field =
    let open Qexp in
    f
      Mapper.(
        couple
        $ token (fun x -> Ok x)
        $ function
        | Node elts ->
          List.map mapper elts |> Validation.Applicative.sequence
        | _ ->
          Error [ Invalid_field field ])
      table
      field
  ;;

  let ziplist mapper table field =
    aux_ziplist list mapper table field
  ;;

  let ziplist_refutable mapper table field =
    aux_ziplist list_refutable mapper table field
  ;;

  let token mapper table field =
    let open Qexp in
    match Hashtbl.find_opt table field with
    | None ->
      Error [ Undefined_field field ]
    | Some (Some x) ->
      (match x with
      | Atom str | String (_, str) | Tag str | Keyword str ->
        mapper str
      | _ ->
        Error [ Invalid_field field ])
    | _ ->
      Error [ Invalid_field field ]
  ;;

  let int table field =
    token
      Validation.(
        int_of_string_opt %> from_option (Invalid_field field))
      table
      field
  ;;

  module D = Timetable.Day

  let day = token Validation.(D.from_string %> from_result)
  let color = token Validation.(Color.from_string %> from_result)
end
