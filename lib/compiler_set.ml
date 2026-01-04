open Types
open Compiler

include Set.Make (struct
  type t = Compiler.t

  let compare a b = String_set.compare a.ext b.ext
end)

let default = of_list Compiler.default
