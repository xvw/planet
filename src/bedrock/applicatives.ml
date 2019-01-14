module List = Applicative.Make_from_monad (Monads.List)
module Option = Applicative.Make_from_monad (Monads.Option)
module Array = Applicative.Make_from_monad (Monads.Array)
module Result = Applicative.Make_from_monad (Monads.Result)
