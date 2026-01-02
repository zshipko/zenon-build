type t = { mutable compile : string list; mutable link : string list }

let v ?(compile = []) ?(link = []) () = { compile; link }
let add_compile_flags t flags = t.compile <- t.compile @ flags
let add_link_flags t flags = t.link <- t.link @ flags
let concat a b = { compile = a.compile @ b.compile; link = a.link @ b.link }
