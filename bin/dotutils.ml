module DotGraph = struct
  type t = { g_label : string; g_arrows : t list }

  let rec dump_elm (uid : int) out (self : t) : int =
    match self with
    | { g_label = s; g_arrows = [] } ->
        Printf.fprintf out "%i [label=\"%s\"];\n" uid s;
        uid + 1
    | { g_label = s; g_arrows = l } ->
        Printf.fprintf out "%i [label=\"%s\"];\n" uid s;
        dump_list uid (uid + 1) out l

  and dump_list (fid : int) (uid : int) out (lst : t list) : int =
    match lst with
    | [] -> uid + 1
    | a :: tl ->
        let uid' = dump_elm uid out a in
        Printf.fprintf out "%i -> %i;\n" fid uid;
        dump_list fid (uid' + 1) out tl

  let dump out self =
    Printf.fprintf out "digraph {\nrankdir=LR;\n";
    let cnt = dump_elm 1 out self in
    Printf.fprintf out "}\n";
    Printf.eprintf "Dumped %i nodes\n" cnt
end
