open Bedrock
open Error
open Baremetal
open Util
open Paperwork
module TT = Timetable

let pcmp f a b =
  let r = f a b in
  if r = 0 then -1 else r
;;

let database = Database.logs
let log_folder = Database.path database
let whereami_file = Filename.concat log_folder "whereami.qube"
let update_table_project = Filename.concat log_folder "projects.qube"

let log_pattern =
  [ Filename.concat log_folder "log_*.qube"; update_table_project ]
;;

let create_file day =
  let month = TT.Day.to_month day in
  let fname = Format.asprintf "log_%a.qube" TT.Month.pp month in
  let cname = Filename.concat log_folder fname in
  let is_already_created = File.exists cname in
  let open Result.Infix in
  (if not is_already_created
  then (
    let header = Format.asprintf ";; Logs for %a" TT.Month.pp month in
    File.create cname header)
  else Ok ())
  >> Ok (cname, is_already_created)
;;

let create_artifact_file file =
  let cname = file in
  let is_already_created = File.exists cname in
  let open Result.Infix in
  (if not is_already_created then File.create cname "" else Ok ())
  >> Ok (cname, is_already_created)
;;

let create_whereami_file () = create_artifact_file whereami_file
let create_update_table_projects () = create_artifact_file update_table_project

let read_logs filename =
  let open Validation.Infix in
  filename |> Filename.concat log_folder |> File.to_string
  |> Result.bind Qexp.from_string
  |> Result.bind Qexp.extract_root
  |> Validation.from_result
  >|= List.map Shapes.Log.from_qexp
  >>= Validation.Applicative.sequence
;;

let read_project_updates () =
  let open Result.Infix in
  create_update_table_projects ()
  >> (update_table_project |> File.to_string
     |> Result.bind Qexp.from_string
     |> Result.bind Shapes.Update_table.from_qexp)
;;

let push_project_updates table =
  let qexp = Shapes.Update_table.to_qexp table in
  let str = Qexp.to_string qexp in
  File.overwrite update_table_project str
;;

let logs_to_json logs =
  let open Paperwork.Json in
  array $ List.map Shapes.Log.to_json logs
;;

let log_cmp log_a log_b =
  let open Shapes.Log in
  Timetable.Day.cmp log_a.day log_b.day
;;

let whereami_to_json ?(reverse = true) () =
  let filename = whereami_file in
  let cmp (a, _, _) (b, _, _) = Timetable.Day.cmp a b in
  let sorter = if reverse then flip $ pcmp cmp else pcmp cmp in
  let open Result.Infix in
  filename
  |> File.to_stream (fun _ -> Qexp.from_stream)
  >>= Qexp.extract_root
  >|= List.map (function
          | Qexp.Node
              [ Qexp.Keyword daypoint
              ; Qexp.String (_, country)
              ; Qexp.String (_, city)
              ] ->
            daypoint |> TT.Day.from_string >|= fun dp -> dp, country, city
          | x ->
            Error (Invalid_field (Qexp.to_string x)))
  >>= Result.Applicative.sequence >|= List.sort sorter
  >|= List.map (fun (daypoint, country, city) ->
          Json.(
            obj
              [ "date", string $ Timetable.Day.to_string daypoint
              ; "country", string country
              ; "city", string city
              ]))
  >|= Json.array |> Validation.from_result
;;

let collect_log_files ?(reverse = true) () =
  let sorter = if reverse then flip String.compare else String.compare in
  let open Result.Infix in
  Dir.children
    ~filter:(fun x -> String.(start_with x "log_" && end_with x ".qube"))
    log_folder
  >|= List.sort sorter
;;

let traverse ?(reverse = true) f default =
  let open Validation.Infix in
  let sorter = if reverse then flip $ pcmp log_cmp else pcmp log_cmp in
  collect_log_files ~reverse ()
  |> Validation.from_result
  >>= fun files ->
  List.fold_left
    (fun potential_acc filename ->
      potential_acc
      >>= fun acc ->
      filename |> Filename.concat log_folder
      |> File.to_stream (fun _ -> Qexp.from_stream)
      |> Result.bind Qexp.extract_root
      |> Validation.from_result
      >>= (fun nodes ->
            List.map (fun x -> Shapes.Log.(from_qexp x)) nodes
            |> Validation.Applicative.sequence >|= List.sort sorter)
      >|= fun nodes -> List.fold_left f acc nodes)
    (Ok default)
    files
;;

let collect_all_log_in_json ?(reverse = true) () =
  let open Validation.Infix in
  traverse ~reverse (fun acc log -> acc @ [ Shapes.Log.to_json log ]) []
  >|= Json.array
;;

let context () =
  let open Validation.Syntax in
  let* table = read_project_updates () |> Validation.from_result in
  let* ctx = Ok (Shapes.Context.init table) in
  let+ result = traverse Shapes.Context.update ctx in
  result
;;
