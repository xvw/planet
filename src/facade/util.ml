open Js_of_ocaml

let window = Dom_html.window
let document = Dom_html.document
let scroll_y () = (Js.Unsafe.coerce window)##.scrollY
let scroll_x () = (Js.Unsafe.coerce window)##.scrollX
let get_by_id = Dom_html.getElementById_opt
let clear element = element##.innerHTML := Js.string ""

let offset_y element =
  let a = element##getBoundingClientRect##.top in
  int_of_float a + scroll_y ()
;;

let watch_once event args f =
  let%lwt result = event args in
  let _ = f result in
  Lwt.return ()
;;

let rec watch event args f =
  let%lwt _ = watch_once event args f in
  watch event args f
;;
