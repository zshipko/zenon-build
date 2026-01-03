open Types
open Compiler

include Set.Make (struct
  type t = Compiler.t

  let compare a b = String_set.compare a.ext b.ext
end)

let default =
  of_list
    [
      Compiler.clang;
      Compiler.clangxx;
      Compiler.ispc;
      Compiler.flang;
      Compiler.ghc;
      Compiler.mlton;
      Compiler.ats2;
    ]

let default_ext =
  fold (fun x acc -> String_set.union x.ext acc) default String_set.empty
