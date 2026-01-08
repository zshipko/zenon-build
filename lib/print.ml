open Plan

module Dot = Graph.Graphviz.Dot (struct
  include G

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name v = "\"" ^ Node.node_id v ^ "\""

  let vertex_attributes v =
    match v with
    | Node.Build b ->
        [ `Shape `Box; `Label b.name; `Style `Filled; `Fillcolor 0xE8F4F8 ]
    | Src s ->
        [
          `Shape `Ellipse;
          `Label (Eio.Path.native_exn s.path |> Filename.basename);
        ]
    | Obj o ->
        [
          `Shape `Note;
          `Label (Eio.Path.native_exn o.path |> Filename.basename);
          `Fillcolor 0xFFF4E6;
          `Style `Filled;
        ]
    | Output p ->
        [
          `Shape `Doubleoctagon;
          `Label (Eio.Path.native_exn p |> Filename.basename);
          `Style `Filled;
          `Fillcolor 0xE8F8E8;
        ]
    | External e ->
        [
          `Shape `Box;
          `Label (e.path ^ ":" ^ e.target);
          `Style `Filled;
          `Fillcolor 0xFFE6E6;
        ]

  let get_subgraph v =
    match v with
    | Node.Build _ -> None
    | Src _ -> None
    | Obj _ -> None
    | Output _ -> None
    | External _ -> None

  let default_edge_attributes _ = []

  let edge_attributes (_, e, _) =
    match e with
    | Some (Node.Script _) ->
        [ `Label "script"; `Color 0x9B59B6; `Style `Dashed ]
    | Some (Compiler (c, _)) ->
        [ `Label c.name; `Color 0x3498DB; `Arrowhead `Normal ]
    | Some (Linker l) -> [ `Label l.name; `Color 0x27AE60; `Penwidth 2.0 ]
    | Some Dependency -> [ `Color 0x000000; `Style `Solid ]
    | None -> [ `Color 0xBDC3C7; `Style `Dotted ]
end)

let to_dot (t : Plan.t) =
  Dot.fprint_graph Format.str_formatter (Plan.graph t);
  Format.flush_str_formatter ()
