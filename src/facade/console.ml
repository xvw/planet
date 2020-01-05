open Js_of_ocaml
open Bedrock
open Util

class type hook =
  object
    inherit Firebug.console

    method clear : unit Js.meth

    method count : Js.js_string Js.t Js.Optdef.t -> unit Js.meth

    method countReset : Js.js_string Js.t Js.Optdef.t -> unit Js.meth

    method timeLog : 'a. Js.js_string Js.t -> 'a -> unit Js.meth

    method table :
      'b. 'b -> Js.js_string Js.t Js.js_array Js.t Js.Optdef.t -> unit Js.meth
  end

external get_console : unit -> hook Js.t = "caml_js_get_console"

let console = get_console ()
let log x = console##log x
let print x = x |> Js.string |> log
let clear () = console##clear
let info x = console##info x
let error x = console##error x
let warning x = console##warn x
let dir x = console##dir x
let trace () = console##trace

let table ?columns obj =
  let opt_columns =
    Js.Optdef.map (Js.Optdef.option columns) (fun columns ->
        List.map Js.string columns |> Array.of_list |> Js.array)
  in
  console##table obj opt_columns
;;

let opt_str str_value =
  str_value |> Js.Optdef.option |> fun x -> Js.Optdef.map x Js.string
;;

let time name = console##time (Js.string name)
let time_log name x = console##timeLog (Js.string name) x
let time_end name = console##timeEnd (Js.string name)
let count ?label () = console##count (opt_str label)
let count_reset ?label () = console##countReset (opt_str label)

let timetrack timer_name actions =
  let name = Js.string timer_name in
  let () = console##time name in
  let () = List.iter (fun f -> f (fun x -> console##timeLog name x)) actions in
  console##timeEnd name
;;

let group ?label () = console##group (Js.Optdef.option label)
let group_end () = console##groupEnd
let render_error errors = errors |> List.iter (Error.to_string %> Js.string %> error)

let dump_errors obj errs =
  error
  $ object%js
      val messages =
        Js.array (List.map (Error.to_string %> Js.string) errs |> Array.of_list)

      val reference = obj
    end
;;
