open Js_of_ocaml
open Bedrock
open Util
open Error
open Paperwork

module Log = struct
  class type js =
    object
      method uuid : Js.js_string Js.t Js.readonly_prop

      method day : Js.js_string Js.t Js.readonly_prop

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
          obj##.day
    <*> pure obj##.duration
    <*> (Js.to_string %> pure) obj##.sector
    <*> (Js.Optdef.to_option %> Option.map Js.to_string %> pure)
          obj##.project
    <*> (Js.to_string %> pure) obj##.label
  ;;

  let process () = Console.print "Process logs"

  let api =
    object%js
      method process = process ()
    end
  ;;
end
