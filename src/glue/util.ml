open Bedrock
open Paperwork

let now () = Unix.gettimeofday () |> Unix.localtime

let current_year () =
  let n = now () in
  n.Unix.tm_year + 1900
;;

let moment_with_sec_of timestamp =
  let mon = timestamp.Unix.tm_mon + 1 in
  let yea = timestamp.Unix.tm_year - 100 in
  let day = timestamp.Unix.tm_mday in
  let h = timestamp.Unix.tm_hour in
  let m = timestamp.Unix.tm_min in
  let s = timestamp.Unix.tm_sec in
  let open Result.Infix in
  mon
  |> Timetable.Month.from_int
  >>= (fun month -> Timetable.Moment.make_with yea month day h m)
  >|= fun m -> m, s
;;

let moment_of ts = ts |> moment_with_sec_of |> Result.map fst
let moment () = moment_of (now ())
let moment_with_sec () = now () |> moment_with_sec_of

let hour_of timestamp =
  let open Result.Infix in
  timestamp |> moment_of >|= Timetable.Moment.extract >|= fun (_, _, _, h) -> h
;;

let hour () = hour_of (now ())

let day_of timestamp =
  let open Result.Infix in
  timestamp |> moment_of >|= Timetable.Moment.extract >|= fun (_, _, d, _) -> d
;;

let day () = day_of (now ())

let month_of timestamp =
  let open Result.Infix in
  timestamp |> moment_of >|= Timetable.Moment.extract >|= fun (_, m, _, _) -> m
;;

let month () = month_of (now ())

let year_of timestamp =
  let open Result.Infix in
  timestamp |> moment_of >|= Timetable.Moment.extract >|= fun (y, _, _, _) -> y
;;

let year () = year_of (now ())
