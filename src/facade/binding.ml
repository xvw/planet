open Js_of_ocaml
open Bedrock
open Util
open Error
module Ajax = Lwt_xmlHttpRequest

module Log = struct
  class type js =
    object
      method uuid : Js.js_string Js.t Js.readonly_prop

      method date : Js.js_string Js.t Js.readonly_prop

      method duration : int Js.readonly_prop

      method sector : Js.js_string Js.t Js.readonly_prop

      method project : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method label : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  let mk uuid day duration sector project label =
    Shapes.Log.{ uuid; day; duration; sector; project; label }
  ;;

  let shape obj =
    let open Validation in
    mk
    <$> (Js.to_string %> pure) obj##.uuid
    <*> (Js.to_string %> Paperwork.Timetable.Day.from_string
       %> from_result)
          obj##.date
    <*> pure obj##.duration
    <*> (Js.to_string %> pure) obj##.sector
    <*> (Js.Optdef.to_option %> Option.map Js.to_string %> pure)
          obj##.project
    <*> (Js.to_string %> pure) obj##.label
  ;;

  let dump_log log =
    let open Shapes.Log in
    let k = "log-" ^ log.uuid in
    let c = to_json %> Paperwork.Json.to_string $ log in
    Storage.Session.set k c
  ;;

  let get_by_id uuid =
    "log-" ^ uuid |> Storage.Session.get
    |> Option.map (Js.string %> Json.unsafe_input)
  ;;

  let reduce_log acc log _i =
    let open Shapes.Log in
    let () = dump_log log in
    ()
  ;;

  let hydrate () =
    let open Lwt.Infix in
    "/api/logs.json" |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= fun x ->
    (Js._JSON##parse x)##reduce_init
      (Js.wrap_callback (fun acc obj i _ ->
           match shape obj with
           | Error errs ->
             let () = Console.dump_errors obj errs in
             acc
           | Ok log ->
             reduce_log acc log i))
      ()
  ;;
end

module Location = struct
  class type js =
    object
      method date : Js.js_string Js.t Js.readonly_prop

      method country : Js.js_string Js.t Js.readonly_prop

      method city : Js.js_string Js.t Js.readonly_prop
    end

  type t = js Js.t

  let mk date country city = date, country, city

  let shape obj =
    let open Validation in
    mk
    <$> (Js.to_string %> Paperwork.Timetable.Day.from_string
       %> from_result)
          obj##.date
    <*> (Js.to_string %> String.capitalize_ascii %> pure)
          obj##.country
    <*> (Js.to_string %> String.capitalize_ascii %> pure) obj##.city
  ;;

  let get () =
    let open Lwt.Infix in
    "/api/whereami.json" |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x -> Js._JSON##parse x)
    >|= Js.to_array %> Array.to_list %> List.map shape
        %> Validation.Applicative.sequence
  ;;
end
