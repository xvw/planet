let call = "./gallery.exe"
let version = "1.0.0"

open Cmdliner

let create =
  let doc = "Create a new Gallery" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  let kind =
    let doc = "Kind of the gallery" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"KIND" ~doc)
  in
  Term.(const Lib.create $ kind, info "new" ~version ~doc ~exits ~man)
;;

let index =
  let doc = "Manage gallery of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  let open Term in
  ret (const (`Help (`Pager, None))), info call ~version ~doc ~exits ~man
;;

let invoke () =
  let open Term in
  exit @@ eval_choice index [ create ]
;;
