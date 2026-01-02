open Types
open Compiler

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String_set.compare a.ext b.ext
  end)

  let default = of_list [ Compiler.clang; Compiler.clangxx; Compiler.ispc ]

  let default_ext =
    fold (fun x acc -> String_set.union x.ext acc) default String_set.empty
end
