let call = "./project.exe"
let version = "1.0.0"

open Cmdliner

let ls =
  let doc = "Show the list of stored projects" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.ls $ const ())
  , Term.info "ls" ~version ~doc ~exits ~man )
;;

let show =
  let project =
    let doc = "Project to be inspected" in
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"PROJECT" ~doc)
  in
  let expand =
    let doc = "Show the article of the project" in
    Arg.(
      value & flag
      & info [ "expand"; "show-content"; "e" ] ~docv:"expand" ~doc)
  in
  let doc = "Show a specific project" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(const Lib.show $ project $ expand)
  , Term.info "show" ~version ~doc ~exits ~man )
;;

let index =
  let doc = "Inspect/Show project of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () = Term.(exit @@ eval_choice index [ ls; show ])
