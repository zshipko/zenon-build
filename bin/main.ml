let () =
  Eio_posix.run @@ fun env ->
  let path =
    if Array.length Sys.argv > 1 then Eio.Path.(env#cwd / Sys.argv.(1))
    else env#cwd
  in
  let x =
    match Zenon.Config.load ~env path with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  Zenon.Build.run x
