open Bedrock
open Error
open Baremetal
open Util
open Paperwork
module TT = Timetable

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

let whereami_to_json () =
  let filename = Filename.concat log_folder "whereami.qube" in
  let open Result.Infix in
  filename |> File.to_string >>= Qexp.from_string
  >>= (function
        | Qexp.Node li ->
          Ok li
        | x ->
          Error (No_root_element (Qexp.to_string x)))
  >|= List.map (function
          | Qexp.Node
              [ Qexp.Keyword daypoint
              ; Qexp.String (_, country)
              ; Qexp.String (_, city) ] ->
            daypoint |> TT.Day.from_string
            >|= fun _ ->
            Json.(
              obj
                [ "date", string daypoint
                ; "country", string country
                ; "city", string city ])
          | x ->
            Error (Invalid_field (Qexp.to_string x)) )
  >>= Result.Applicative.sequence >|= Json.array
  |> Validation.from_result
;;
