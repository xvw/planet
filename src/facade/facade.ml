open Js_of_ocaml

let start f = Dom_html.window##.onload := f

let () =
  Js.export
    "planet"
    (object%js (self)
       val internal =
         object%js
           val loadTasks = Js.array [||]
         end

       method suspend f =
         let _ = self##.internal##.loadTasks##push f in
         ()

       method start =
         let f =
           self##.internal##.loadTasks
           |> Js.to_array
           |> Array.fold_left
                (fun callback task () ->
                  let () = callback () in
                  Js.Unsafe.fun_call task [||])
                (fun () -> ())
           |> function
           | callback ->
             fun _ ->
               let () = callback () in
               Js._true
         in
         start (Dom.handler f)

       val project = Widget.Project.api

       val common = Widget.Common.api

       val roe = Roe.api
    end)
;;
