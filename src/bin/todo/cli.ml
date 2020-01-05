let call = "./todo.exe"
let version = "1.0.0"

open Cmdliner

let create =
  let doc = "Create a new task" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  (Term.(const Lib.create $ const ()), Term.info "new" ~version ~doc ~exits ~man)
;;

let show =
  let doc = "Show a task" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  let taskname =
    let doc = "Task to be inspected" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TASKNAME" ~doc)
  in
  (Term.(const Lib.show $ taskname), Term.info "show" ~version ~doc ~exits ~man)
;;

let index =
  let doc = "Manage build process of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info call ~version ~doc ~exits ~man )
;;

let invoke () = Term.(exit @@ eval_choice index [ create; show ])