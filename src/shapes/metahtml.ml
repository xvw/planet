type t = (string * string) list
type html = string

let classify = function
  | [] -> ""
  | classes -> " " ^ String.concat " " classes
;;

let to_html classes data =
  Format.asprintf {|<span class="planet-metadata%s"|} (classify classes)
  ^ List.fold_left
      (fun rest (key, value) -> Format.asprintf {|%sdata-%s="%s"|} rest key value)
      ""
      data
  ^ "><!-- Planet metadata --></span>\n"
;;
