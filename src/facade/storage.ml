open Js_of_ocaml

exception Not_supported
exception Not_found

type t = Dom_html.storage Js.t

class type storageEvent =
  object
    inherit Dom_html.event

    method key : Js.js_string Js.t Js.opt Js.readonly_prop

    method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop

    method newValue : Js.js_string Js.t Js.opt Js.readonly_prop

    method url : Js.js_string Js.t Js.readonly_prop

    method storageArea : Dom_html.storage Js.t Js.opt Js.readonly_prop
  end

type event = storageEvent Js.t

let event = Dom.Event.make "storage"

module type STORAGE = sig
  type key = string
  type value = string
  type old_value = string
  type url = string

  type change_state =
    | Clear
    | Insert of key * value
    | Remove of key * old_value
    | Update of key * old_value * value

  val dump_change_state : change_state -> string
  val is_supported : unit -> bool
  val handler : t
  val length : unit -> int
  val get : key -> value option
  val set : key -> value -> unit
  val remove : key -> unit
  val clear : unit -> unit
  val key : int -> key option
  val at : int -> (key * value) option
  val to_hashtbl : unit -> (key, value) Hashtbl.t
  val iter : (key -> value -> unit) -> unit
  val find : (key -> value -> bool) -> (key * value) option
  val select : (key -> value -> bool) -> (key, value) Hashtbl.t

  val on_change :
    ?prefix:string -> (change_state -> url -> unit) -> Dom.event_listener_id

  val on_clear : (url -> unit) -> Dom.event_listener_id

  val on_insert :
    ?prefix:string -> (key -> value -> url -> unit) -> Dom.event_listener_id

  val on_remove :
    ?prefix:string -> (key -> old_value -> url -> unit) -> Dom.event_listener_id

  val on_update :
       ?prefix:string
    -> (key -> old_value -> value -> url -> unit)
    -> Dom.event_listener_id
end

module Make (R : sig
  val handler : t Js.optdef
end) : STORAGE = struct
  type key = string
  type value = string
  type old_value = string
  type url = string

  type change_state =
    | Clear
    | Insert of key * value
    | Remove of key * old_value
    | Update of key * old_value * value

  let dump_change_state = function
    | Clear ->
      "Clear"
    | Insert (k, v) ->
      Printf.sprintf "Insert (%s, new_value: %s)" k v
    | Remove (k, v) ->
      Printf.sprintf "Remove (%s, old_value: %s)" k v
    | Update (k, v, c) ->
      Printf.sprintf "Update (%s, %s --> %s)" k v c
  ;;

  let is_supported () =
    (match Js.Optdef.to_option R.handler with Some _ -> true | None -> false)
  ;;

  let handler =
    Js.Optdef.case R.handler (fun () -> raise Not_supported) (fun x -> x)
  ;;

  let length () = handler##.length

  let get key =
    handler##getItem (Js.string key)
    |> Js.Opt.to_option
    |> Option.map Js.to_string
  ;;

  let set key value =
    let k = Js.string key in
    let v = Js.string value in
    handler##setItem k v
  ;;

  let remove key = handler##removeItem (Js.string key)
  let clear () = handler##clear
  let key i = handler##key i |> Js.Opt.to_option |> Option.map Js.to_string

  let at i =
    match key i with
    | None ->
      None
    | Some k ->
      Option.map (fun e -> (k, e)) (get k)
  ;;

  let iter f =
    let len = length () in
    for i = 0 to pred len do
      (match at i with None -> raise Not_found | Some (k, v) -> f k v)
    done
  ;;

  let to_hashtbl () =
    let len = length () in
    let hash = Hashtbl.create len in
    let () = iter (Hashtbl.add hash) in
    hash
  ;;

  let find f =
    let len = length () in
    let rec loop i =
      if i = len then
        None
      else (
        match at i with
        | None ->
          raise Not_found
        | Some (k, v) ->
          if f k v then Some (k, v) else loop (succ i)
      ) in
    loop 0
  ;;

  let select f =
    let hash = Hashtbl.create 16 in
    iter (fun k v -> if f k v then Hashtbl.add hash k v);
    hash
  ;;

  let is_valid_storage ev = Js.Opt.return handler = ev##.storageArea
  let begin_by prefix str = str##lastIndexOf_from (Js.string prefix) 0 = 0

  let make_change k ev =
    let key = Js.to_string k in
    let value = Js.Opt.to_option ev##.newValue in
    let old = Js.Opt.to_option ev##.oldValue in
    match (old, value) with
    | (None, Some x) ->
      Insert (key, Js.to_string x)
    | (Some x, None) ->
      Remove (key, Js.to_string x)
    | (Some x, Some y) ->
      Update (key, Js.to_string x, Js.to_string y)
    | (None, None) ->
      Clear
  ;;

  let on_change ?(prefix = "") f =
    let callback (ev : event) =
      if is_valid_storage ev then (
        let url = Js.to_string ev##.url in
        match Js.Opt.to_option ev##.key with
        | None ->
          f Clear url
        | Some k ->
          if begin_by prefix k then f (make_change k ev) url
      );
      Js._true in
    Dom.addEventListener Dom_html.window event (Dom.handler callback) Js._true
  ;;

  let on_clear f =
    on_change (fun ev url -> (match ev with Clear -> f url | _ -> ()))
  ;;

  let on_insert ?(prefix = "") f =
    on_change ~prefix (fun ev url ->
        (match ev with Insert (key, value) -> f key value url | _ -> ()))
  ;;

  let on_remove ?(prefix = "") f =
    on_change ~prefix (fun ev url ->
        match ev with
        | Remove (key, old_value) ->
          f key old_value url
        | _ ->
          ())
  ;;

  let on_update ?(prefix = "") f =
    on_change ~prefix (fun ev url ->
        match ev with
        | Update (key, old_value, new_value) ->
          f key old_value new_value url
        | _ ->
          ())
  ;;
end

module Local = Make (struct
  let handler = Dom_html.window##.localStorage
end)

module Session = Make (struct
  let handler = Dom_html.window##.sessionStorage
end)
