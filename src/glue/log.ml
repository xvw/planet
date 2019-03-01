open Bedrock
open Baremetal
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
