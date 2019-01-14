module List = Monad.Make_with_join (struct
  type 'a t = 'a list

  let return x = [x]
  let map = Stdlib.List.map
  let join = Stdlib.List.concat
end)

module Option = Monad.Make_with_bind (struct
  type 'a t = 'a option

  let return x = Some x
  let bind f = function None -> None | Some x -> f x
end)

module Array = Monad.Make_with_bind (struct
  type 'a t = 'a array

  let return x = [|x|]

  let bind f x =
    Array.fold_right (fun x acc -> Array.concat [f x; acc]) x [||]
  ;;
end)

module Result = Monad.Make_with_bind (struct
  type 'a t = 'a Result.t

  let return x = Ok x
  let bind f = function Error x -> Error x | Ok x -> f x
end)
