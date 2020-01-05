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

let collect_sectors f = function
  | Qexp.Node potential_sectors ->
    let open Validation in
    potential_sectors |> List.map Shapes.Sector.from_qexp |> Applicative.sequence >|= f
  | qexp -> Error [ No_root_element (Qexp.to_string qexp) ]
;;

let all () =
  let open Validation in
  let hash = Hashtbl.create 2 in
  database
  |> Database.path
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result
  >>= collect_sectors (fun sectors -> List.fold_left reducer hash sectors)
;;

let to_json () =
  let open Validation in
  database
  |> Database.path
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result
  >>= collect_sectors id
  >|= fun sectors ->
  Json.obj $ List.map (fun sector -> Shapes.Sector.(sector.name, to_json sector)) sectors
;;

let compute_html_node acc sector =
  let open Shapes.Sector in
  acc
  ^ Shapes.Metahtml.to_html
      [ "sector-data" ]
      [ "name", sector.name
      ; "desc", sector.desc
      ; "color", Paperwork.Color.to_rgb sector.color
      ]
;;

let to_html () =
  let open Validation in
  database
  |> Database.path
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result
  >>= collect_sectors id
  >|= List.fold_left compute_html_node ""
  |> function
  | Ok x -> x
  | _ -> ""
;;
