open Js_of_ocaml
open Bedrock.Util

module Array = struct
  let empty () = new%js Js.array_empty

  let from_list f list =
    let js_array = empty () in
    let () =
      Stdlib.List.iteri
        (fun i elt -> Js.array_set js_array i $ f elt)
        list
    in
    js_array
  ;;
end

module Promise = struct
  let wakeup w x _ =
    let _ = Lwt.wakeup w () in
    x
  ;;

  let wrap f x = Lwt.return (f x)
  let run promise f elt = Lwt.bind (promise elt) (wrap f)

  let onload elt () =
    let thread, wakener = Lwt.wait () in
    let _ = elt##.onload := Dom.handler (wakeup wakener Js._true) in
    thread
  ;;

  let dom_onload = onload Dom_html.window
end
