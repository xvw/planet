type _ t = string

let path database = Filename.concat "." database
let projects = "projects"

let read f database =
  let name = path database in
  f name
;;
