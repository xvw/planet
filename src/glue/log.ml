open Bedrock
open Error
open Baremetal
open Util
open Paperwork
module TT = Timetable

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
    let header =
      Format.asprintf ";; Logs for %a" TT.Month.pp month
    in
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

let create_update_table_projects () =
  create_artifact_file update_table_project
;;

let read_logs filename =
  let open Validation.Infix in
  filename
  |> Filename.concat log_folder
  |> File.to_string
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

let whereami_to_json () =
  let filename = whereami_file in
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
            daypoint |> TT.Day.from_string
            >|= fun _ ->
            Json.(
              obj
                [ "date", string daypoint
                ; "country", string country
                ; "city", string city
                ])
          | x ->
            Error (Invalid_field (Qexp.to_string x)))
  >>= Result.Applicative.sequence >|= Json.array
  |> Validation.from_result
;;

let collect_log_files () =
  let open Result.Infix in
  Dir.children
    ~filter:(fun x ->
      String.(start_with x "log_" && end_with x ".qube"))
    log_folder
  >|= List.sort String.compare
;;

let reduce_logs_json racc filename =
  let open Validation.Infix in
  racc
  >>= fun acc ->
  filename
  |> Filename.concat log_folder
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> Result.bind Qexp.extract_root
  |> Validation.from_result
  >>= fun nodes ->
  List.map (fun x -> Shapes.Log.(from_qexp x >|= to_json)) nodes
  |> Validation.Applicative.sequence
  >|= fun result -> acc @ result
;;

let collect_all_log_in_json () =
  let open Validation.Infix in
  collect_log_files () |> Validation.from_result
  >>= List.fold_left reduce_logs_json (Ok [])
  >|= Json.array
;;

let traverse f default =
  let open Validation.Infix in
  collect_log_files () |> Validation.from_result
  >>= fun files ->
  List.fold_left
    (fun potential_acc filename ->
      potential_acc
      >>= fun acc ->
      filename
      |> Filename.concat log_folder
      |> File.to_stream (fun _ -> Qexp.from_stream)
      |> Result.bind Qexp.extract_root
      |> Validation.from_result
      >>= (fun nodes ->
            List.map (fun x -> Shapes.Log.(from_qexp x)) nodes
            |> Validation.Applicative.sequence)
      >|= fun nodes -> List.fold_left f acc nodes)
    (Ok default)
    files
;;
