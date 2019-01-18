module S = Stdlib.String

let md5 = Util.md5

let start_with base suffix =
  let len_base = S.length base in
  let len_suff = S.length suffix in
  if len_base = len_suff && len_base = len_suff
  then true
  else if len_base < len_suff
  then false
  else
    let rec loop i =
      if i >= len_suff
      then true
      else if S.get base i = S.get suffix i
      then loop (succ i)
      else false
    in
    loop 0
;;

let end_with base suffix =
  let len_base = S.length base in
  let len_suff = S.length suffix in
  if len_base = len_suff && len_base = len_suff
  then true
  else if len_base < len_suff
  then false
  else
    let offset_i = len_base - len_suff in
    let rec loop i =
      if i >= len_suff
      then true
      else if S.get base (offset_i + i) = S.get suffix i
      then loop (succ i)
      else false
    in
    loop 0
;;

let has_extension base extension = end_with base ("." ^ extension)

include S
