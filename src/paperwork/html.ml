type attr =
  | Flag of string
  | Pair of string * string

type node =
  | Leaf of attr list
  | Node of (attr list * node list)
