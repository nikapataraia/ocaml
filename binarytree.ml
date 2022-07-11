type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop


let t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))
let t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty))
let tree = Node (4, t_l , t_r)




(*let crawl comdlst startertree = 
  let nodestack = Stack.create() in
  let rec brain remainingcmd tree= 
    match remainingcmd with 
      [] -> tree  
    | h::t -> match h with 
      | Left-> let Node (v,l,r) = tree in Node (v, (brain t l), r) 
      | Right -> let Node (v,l,r) = tree in Node (v, l, (brain t r))  
      | Delete -> brain t Empty
      | New x -> brain  t (Node (x, Empty,Empty))
      | Push -> Stack.push tree nodestack; brain t tree
      | Pop -> let top = Stack.pop nodestack in brain t top 
      | Up -> WRITING UP BY THIS WAY IS REALY HARD OR IMPOSSIBLE, OK NOT IMPOSSIBLE BUT REALLY HARD :DDDD
  in brain comdlst startertree;;*)


let crawl cmds tree = 
  let nodestack = Stack.create() in
  let savednodes = Stack.create() in
  let rec brain intree remainingcomd =
    match remainingcomd with [] -> if Stack.is_empty nodestack then intree else brain intree [Up]
    |h::t -> match h with 
     | Left -> let Node(v,l,r) = intree in Stack.push (intree,Left) nodestack ; brain l t 
     | Right -> let Node (v,l,r) = intree in Stack.push (intree,Right) nodestack ; brain r t 
     | Delete -> brain Empty t
     | New x -> brain (Node(x,Empty,Empty)) t
     | Push -> Stack.push intree savednodes; brain intree t
     | Pop -> brain (Stack.pop savednodes) t
     | Up -> let pred = Stack.pop nodestack in 
       let (x,y) = pred in
       if (y=Left) then let Node(v,l,r) = x in brain (Node(v,intree,r)) t
       else let Node(v,l,r) = x in brain (Node(v,l,intree)) t in
  brain tree cmds