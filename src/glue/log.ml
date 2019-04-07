open Bedrock
open Error
open Baremetal
open Util
module TT = Paperwork.Timetable

let database = Database.logs
let log_folder = Database.path database

let create_file day =
  let month = TT.Day.to_month day in
  let fname = Format.asprintf "log_%a.qube" TT.Month.pp month in
  let cname = Filename.concat log_folder fname in
  let is_already_created = File.exists cname in
  let open Result.Infix in
  (if not is_already_created
  then
    let header =
      Format.asprintf ";; Logs for %a" TT.Month.pp month
    in
    File.create cname header
  else Ok ())
  >> Ok (cname, is_already_created)
;;

let create_whereami_file () =
  let cname = Filename.concat log_folder "whereami.qube" in
  let is_already_created = File.exists cname in
  let open Result.Infix in
  (if not is_already_created then File.create cname "" else Ok ())
  >> Ok (cname, is_already_created)
;;

let read_logs filename =
  let open Validation.Infix in
  filename
  |> Filename.concat log_folder
  |> File.to_string
  |> Result.bind Paperwork.Qexp.from_string
  |> Validation.from_result
  >>= (function
        | Node x ->
          Ok x
        | node ->
          Error [Not_a_valid_node (Paperwork.Qexp.to_string node)])
  >|= List.map Shapes.Log.from_qexp
  >>= Validation.Applicative.sequence
;;

let logs_to_json logs =
  let open Paperwork.Json in
  array $ List.map Shapes.Log.to_json logs
;;
