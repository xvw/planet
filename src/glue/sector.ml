open Bedrock
open Baremetal
open Paperwork
open Error
open Util

let database = Database.sectors
let name_of s = Shapes.Sector.(s.name)

let reducer hashtable sector =
  let () = Hashtbl.add hashtable $ name_of sector $ sector in
  hashtable
;;

let collect_sectors = function
  | Qexp.Node potential_sectors ->
    let open Validation in
    let sectors = Hashtbl.create 2 in
    potential_sectors
    |> List.map Shapes.Sector.from_qexp
    |> Applicative.sequence
    >|= List.fold_left reducer sectors
  | qexp ->
    Error [No_root_element (Qexp.to_string qexp)]
;;

let all () =
  let open Validation in
  database |> Database.path
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result >>= collect_sectors
;;
