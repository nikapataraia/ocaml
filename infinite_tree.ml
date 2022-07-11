type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
let layer_tree a = 
  let rec construct b =
    LNode (b , (fun () -> construct (b+1)) , (fun () -> construct (b+1) )) in
  construct a

let test_tree  = 
    let rec construct b =
      LNode (b , (fun () -> construct (2*b)) , (fun () -> construct (3*b) )) in
    construct 1
let interval_tree a b =
  let rec construct c d =
    LNode ((c,d) ,(fun () -> construct c ((c+.d)/.2.)) , (fun () -> construct ((c+.d)/.2.)  d)) in
  construct a b
let rational_tree a b =
  let rec construct c d =
    LNode ((c,d) ,(fun () -> construct c (d+1)) , (fun () -> construct (c+1)  d)) in
  construct a b
let top a tree = 
  let rec build a ltree = 
    match ltree with 
     LNode (al,bl,cl) -> if a <= 0 then Empty
     else Node (al,(build (a-1) (bl())),(build (a-1) (cl())))
  in
  build a tree

let rec map f t = 
  let LNode(a,b,c) = t in 
  LNode( (f a) , (fun() -> map f (b())), (fun() -> map f (c())) )

let find f tree =
  let rec ffind que =
    match que with
    LNode(a,b,c)::t -> if f a then LNode(a,b,c)
    else ffind (t@[b();c()]) in
  ffind [tree]