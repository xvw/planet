let call = "./build.exe"
let version = "1.0.0"

open Cmdliner

let clean =
  let doc = "Clean the generated web app" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.clean_generated $ const ())
  , Term.info "clean" ~version ~doc ~exits ~man )
;;

let init =
  let doc = "Initialize the target for generation" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.generate $ const ())
  , Term.info "init" ~version ~doc ~exits ~man )
;;

let index =
  let doc = "Manage build process of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () = Term.(exit @@ eval_choice index [clean; init])
