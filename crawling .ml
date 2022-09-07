
let crawl cmds tree =
  let rec impl cmds stack tree =
    match cmds, stack, tree with
    | [], s, t -> false, [], s, t
    | Up::cs, ss, t -> true, cs, ss, t
    | Left::cs, ss, Node (x, l, r) ->
      let u, nc, ns, nt = impl cs ss l in
      if u then impl nc ns (Node (x, nt, r)) else u, nc, ns, Node (x, nt, r)
    | Right::cs, ss, Node (x, l, r) ->
      let u, nc, ns, nt = impl cs ss r in
      if u then impl nc ns (Node (x, l, nt)) else u, nc, ns, Node (x, l, nt)
    | New v::cs, ss, _ -> impl cs ss (Node (v, Empty, Empty))
    | Delete::cs, ss, _ -> impl cs ss Empty
    | Push::cs, ss, t -> impl cs (t::ss) t
    | Pop::cs, s::ss, _ -> impl cs ss s
    | _ -> failwith "unexpected"
  in
  let _,_,_, t = impl cmds [] tree in t



