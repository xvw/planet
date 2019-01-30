let call = "./project.exe"
let version = "1.0.0"

open Cmdliner

let ls =
  let doc = "Show the list of stored projects" in
  let man = Glue.Man.default call in
  ( Term.(const Lib.ls $ const ())
  , Term.info "ls" ~version ~doc ~exits:Term.default_exits ~man )
;;

let index =
  let doc = "Inspect/Show project of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () = Term.(exit @@ eval_choice index [ls])
