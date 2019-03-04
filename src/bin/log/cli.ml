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

let record =
  let doc = "Write a log in a CLI mode" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  let sector =
    let doc = "The sector of the task" in
    Arg.(value & opt (some string) None & info ["s"; "sector"] ~doc)
  in
  let duration =
    let doc = "The duration of the task" in
    Arg.(
      value & opt (some int) (Some 60) & info ["d"; "duration"] ~doc)
  in
  let timecode =
    let doc = "The moment of the task" in
    Arg.(value & opt (some string) None & info ["a"; "at"] ~doc)
  in
  let project =
    let doc = "The related project of the task" in
    Arg.(value & opt (some string) None & info ["p"; "project"] ~doc)
  in
  let label =
    let doc = "The label of the task" in
    Arg.(non_empty & pos_all string [] & info [] ~doc)
  in
  ( Term.(
      const Lib.record $ sector $ duration $ timecode $ project
      $ label)
  , Term.info "record" ~version ~doc ~exits ~man )
;;

let index =
  let doc = "Manage logs for timetracking" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () =
  Term.(exit @@ eval_choice index [sectors; interactive; record])
;;
