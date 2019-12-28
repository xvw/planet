module Binutil = Glue.Binutil

let create () =
  Binutil.ensure_sectors_projects (fun _sectors (_ctx, projects) ->
      let _some_project =
        Binutil.may_project (List.map (fun (x, _, _) -> x) projects) in
      let _sectors = () in
      ())
;;
