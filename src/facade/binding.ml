open Js_of_ocaml
open Bedrock
open Util
open Error
open Paperwork.Timetable
module Ajax = Js_of_ocaml_lwt.XmlHttpRequest

let map_list f x = Js.to_array x |> Array.to_seq |> Seq.map f |> List.of_seq
let string_list x = map_list Js.to_string x

module Project = struct
  class type short_js =
    object
      method name : Js.js_string Js.t Js.readonly_prop
      method published : bool Js.t Js.Optdef.t Js.readonly_prop
    end

  type short = short_js Js.t

  let short_shape obj =
    ( Js.to_string obj##.name
    , Js.Optdef.case obj##.published (fun () -> true) Js.to_bool )
  ;;

  let get () =
    let open Lwt.Infix in
    "/api/projects.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x -> Js._JSON##parse x)
    >|= Js.to_array %> Array.to_seq %> Seq.map short_shape
    >|= Hashtbl.of_seq
  ;;
end

module Log = struct
  class type js =
    object
      method uuid : Js.js_string Js.t Js.readonly_prop
      method date : Js.js_string Js.t Js.readonly_prop
      method duration : int Js.readonly_prop
      method sector : Js.js_string Js.t Js.readonly_prop
      method project : Js.js_string Js.t Js.Opt.t Js.readonly_prop
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
    <*> (Js.to_string %> Paperwork.Timetable.Day.from_string %> from_result)
          obj##.date
    <*> pure obj##.duration
    <*> (Js.to_string %> pure) obj##.sector
    <*> (Js.Opt.to_option %> Option.map Js.to_string %> pure) obj##.project
    <*> (Js.to_string %> pure) obj##.label
  ;;

  let dump_log log =
    let open Shapes.Log in
    let k = "log-" ^ log.uuid in
    let c = to_json %> Paperwork.Json.to_string $ log in
    Storage.Local.set k c
  ;;

  let get_by_id uuid =
    "log-" ^ uuid
    |> Storage.Local.get
    |> Option.map (Js.string %> Json.unsafe_input)
  ;;

  let reduce_log acc log _i =
    let open Shapes.Log in
    let () = dump_log log in
    ()
  ;;

  let hydrate () =
    let open Lwt.Infix in
    "/api/logs.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= fun x ->
    (Js._JSON##parse x)##reduce_init
      (Js.wrap_callback (fun acc obj i _ ->
           match shape obj with
           | Error errs ->
             let () = Console.dump_errors obj errs in
             acc
           | Ok log -> reduce_log acc log i))
      ()
  ;;

  let collect () =
    let open Lwt.Infix in
    "/api/logs.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >>= fun x ->
    let parsed = Js.(_JSON##parse x |> Js.to_array) in
    let hash = Hashtbl.create 1 in
    Array.fold_left
      (fun promise_h obj ->
        promise_h
        >|= fun h ->
        match shape obj with
        | Error errs ->
          let () = Console.dump_errors obj errs in
          let () =
            Js.raise_js_error (new%js Js.error_constr (Js.string "Aie Aie"))
          in
          h
        | Ok log ->
          let open Shapes.Log in
          let key = Format.asprintf "%a" Day.ppr log.day in
          (match Hashtbl.find_opt h key with
          | None ->
            let () = Hashtbl.add h key [ log ] in
            h
          | Some acc ->
            let () = Hashtbl.remove h key in
            let () = Hashtbl.add h key (log :: acc) in
            h))
      (Lwt.return hash)
      parsed
  ;;

  let get_last_logs () =
    let open Lwt.Infix in
    "/api/last_logs.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x ->
          let p = Js._JSON##parse x in
          let r = Js.to_array p in
          Array.fold_left
            (fun acc obj ->
              match shape obj with
              | Error errs ->
                let () = Console.dump_errors obj errs in
                acc
              | Ok log -> log :: acc)
            []
            r)
    >|= List.rev
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
    <$> (Js.to_string %> Paperwork.Timetable.Day.from_string %> from_result)
          obj##.date
    <*> (Js.to_string %> String.capitalize_ascii %> pure) obj##.country
    <*> (Js.to_string %> String.capitalize_ascii %> pure) obj##.city
  ;;

  let get () =
    let open Lwt.Infix in
    "/api/whereami.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x -> Js._JSON##parse x)
    >|= Js.to_array
        %> Array.to_list
        %> List.map shape
        %> Validation.Applicative.sequence
  ;;
end

module Tags = struct
  type tags = Js.js_string Js.t Js.js_array Js.t

  class type content =
    object
      method title : Js.js_string Js.t Js.readonly_prop
      method section : Js.js_string Js.t Js.readonly_prop
      method id : Js.js_string Js.t Js.readonly_prop
      method date : Js.js_string Js.t Js.readonly_prop
      method description : Js.js_string Js.t Js.readonly_prop
      method tags : tags Js.readonly_prop
    end

  class type js =
    object
      method allTags : tags Js.readonly_prop
      method contents : content Js.t Js.js_array Js.t Js.readonly_prop
    end

  type t = js Js.t

  let shape (obj : t) =
    let open Validation.Infix in
    let tags = string_list obj##.allTags in
    let content =
      Js.to_array obj##.contents
      |> Array.to_seq
      |> Seq.map (fun o ->
             o##.date
             |> Js.to_string
             |> Day.from_string
             |> Validation.from_result
             >|= fun date ->
             let open Shapes.Tag in
             { title = Js.to_string o##.title
             ; section = Js.to_string o##.section
             ; id = Js.to_string o##.id
             ; date
             ; description = Js.to_string o##.description
             ; tags = string_list o##.tags
             })
      |> List.of_seq
      |> Validation.Applicative.sequence
    in
    content >|= fun ctn -> Shapes.Tag.{ all_tags = tags; contents = ctn }
  ;;

  let get () =
    let open Lwt.Infix in
    "/api/tags.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x -> Js._JSON##parse x)
    >|= shape
  ;;
end

module Tasks = struct
  class type checkable =
    object
      method checked : bool Js.t Js.readonly_prop
      method label : Js.js_string Js.t Js.readonly_prop
    end

  class type task =
    object
      method state : Js.js_string Js.t Js.readonly_prop
      method uuid : Js.js_string Js.t Js.readonly_prop
      method project : Js.js_string Js.t Js.Opt.t Js.readonly_prop
      method sectors : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
      method name : Js.js_string Js.t Js.readonly_prop
      method description : Js.js_string Js.t Js.readonly_prop
      method checklist : checkable Js.t Js.js_array Js.t Js.readonly_prop
      method tags : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
      method date : Js.js_string Js.t Js.readonly_prop
      method openingDate : Js.js_string Js.t Js.Opt.t Js.readonly_prop
      method closingDate : Js.js_string Js.t Js.Opt.t Js.readonly_prop
      method engagementDate : Js.js_string Js.t Js.Opt.t Js.readonly_prop
    end

  class type boardLine =
    object
      method total : int Js.readonly_prop
      method tasks : task Js.t Js.js_array Js.t Js.readonly_prop
    end

  class type board =
    object
      method backlog : boardLine Js.t Js.readonly_prop
      method opened : boardLine Js.t Js.readonly_prop
      method inProgress : boardLine Js.t Js.readonly_prop
      method isDone : boardLine Js.t Js.readonly_prop
      method blocked : boardLine Js.t Js.readonly_prop
    end

  let shape_checklist (obj : checkable Js.t Js.js_array Js.t) =
    Ok (map_list (fun x -> Js.to_bool x##.checked, Js.to_string x##.label) obj)
  ;;

  let opt_date p_date =
    match Js.Opt.to_option p_date with
    | None -> Ok None
    | Some date ->
      date
      |> Js.to_string
      |> Paperwork.Timetable.Day.from_string
      |> Validation.from_result
      |> Validation.map (fun x -> Some x)
  ;;

  let shape_task (obj : task Js.t) =
    let open Validation in
    let open Shapes.Task in
    new_task
    <$> (Js.to_string obj##.state |> state_from_string)
    <*> Ok (Js.to_string obj##.uuid)
    <*> Ok Option.(Js.Opt.to_option obj##.project >|= Js.to_string)
    <*> Ok (string_list obj##.sectors)
    <*> Ok (Js.to_string obj##.name)
    <*> Ok (Js.to_string obj##.description)
    <*> shape_checklist obj##.checklist
    <*> Ok (string_list obj##.tags)
    <*> (obj##.date
        |> Js.to_string
        |> Paperwork.Timetable.Day.from_string
        |> from_result)
    <*> opt_date obj##.openingDate
    <*> opt_date obj##.closingDate
    <*> opt_date obj##.engagementDate
  ;;

  let shape_line (obj : boardLine Js.t) =
    let open Validation in
    map_list shape_task obj##.tasks
    |> Applicative.sequence
    >|= fun tasks -> obj##.total, tasks
  ;;

  let shape (obj : board Js.t) =
    let open Validation in
    let open Shapes.Task in
    new_board
    <$> shape_line obj##.backlog
    <*> shape_line obj##.opened
    <*> shape_line obj##.inProgress
    <*> shape_line obj##.isDone
    <*> shape_line obj##.blocked
  ;;

  let get () =
    let open Lwt.Infix in
    "/api/tasks.json"
    |> Ajax.get
    >|= (fun frame -> frame.Ajax.content)
    >|= Js.string
    >|= (fun x -> Js._JSON##parse x)
    >|= shape
  ;;
end
