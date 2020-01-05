let call = "./build.exe"
let version = "1.0.0"

open Cmdliner

let init =
  let doc = "Initialize the target for generation" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.generate $ const ()), Term.info "init" ~version ~doc ~exits ~man
;;

let api =
  let doc = "Initialize the target for API generation" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.api $ const ()), Term.info "api" ~version ~doc ~exits ~man
;;

let projects =
  let doc = "Initialize the target for Project generation" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.base_project $ const ()), Term.info "projects" ~version ~doc ~exits ~man
;;

let stories =
  let doc = "Build stories from Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.stories $ const ()), Term.info "stories" ~version ~doc ~exits ~man
;;

let location =
  let doc = "Build location from Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.location $ const ()), Term.info "location" ~version ~doc ~exits ~man
;;

let all =
  let doc = "Build everything" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.all $ const ()), Term.info "all" ~version ~doc ~exits ~man
;;

let twtxt =
  let doc = "Build twtxt feeds" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(const Lib.twtxt $ const ()), Term.info "twtxt" ~version ~doc ~exits ~man
;;

let index =
  let doc = "Manage build process of Planet" in
  let man = Glue.Man.default call in
  let exits = Term.default_exits in
  Term.(ret (const (`Help (`Pager, None)))), Term.info call ~version ~doc ~exits ~man
;;

let invoke () =
  Term.(exit @@ eval_choice index [ all; init; api; projects; stories; twtxt ])
;;
