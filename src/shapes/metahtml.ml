type t = (string * string) list
type html = string

let to_html data =
  {|<span class="planet-metadata"|}
  ^ List.fold_left
      (fun rest (key, value) ->
        Format.asprintf {|%sdata-%s="%s"|} rest key value)
      ""
      data
  ^ "><!-- Planet metadata --></span>\n"
;;
