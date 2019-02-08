let call = "./log.exe"
let version = "1.0.0"

open Cmdliner

let sectors =
  let doc = "Show the list of available sectors" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.sectors $ const ())
  , Term.info "sectors" ~version ~doc ~exits ~man )
;;

let interactive =
  let doc = "Write a log in interactive mode" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.interactive $ const ())
  , Term.info "interactive" ~version ~doc ~exits ~man )
;;

let index =
  let doc = "Manage logs for timetracking" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () =
  Term.(exit @@ eval_choice index [sectors; interactive])
;;
