open Cmdliner

let author_email = function None -> "" | Some x -> Format.sprintf " (%s)" x

let render_authors authors =
  authors
  |> List.map (fun (a, b, c) ->
         `P (Format.sprintf "%s - %s%s" a b (author_email c)))
;;

let default ?(other_authors = []) name =
  let authors =
    ("xvw", "Xavier Van de Woestyne", Some "xaviervdw@gmail.com")
    :: other_authors in
  [ `S Manpage.s_name; `P name; `S Manpage.s_authors ]
  @ render_authors authors
  @ [ `S Manpage.s_bugs
    ; `P
        "Report bugs/issues/improvements here : \
         <https://github.com/xvw/planet/issues>"
    ]
;;
