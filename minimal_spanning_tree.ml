type graph = (int * float * int) list
(* let checkifconnected lst vertex ind = 
  List.fold_left (fun x y ->let ind = ind + 1 in let (a,b,c) = y in if a = vertex then let(t,k) = x in  t = b ; k = ind
   else if c = vertex then let(t,k) =x in t = b;k = ind else x) (0.,-1) lst  *)


(* let mst graph = 
  let splitgraph ver onnode lst = 
    List.partition (fun (a,_,c) -> (List.mem a onnode && c = ver) || (List.mem c onnode && a = ver)) lst  in


  let rec searchmin lst = 
    List.fold_left (fun x y -> let (a,b,c) = y in let (a1,b1,c1) = x in if b1 > b then y else x) (List.hd lst) (List.tl lst) in


  let addedge e lst = e::lst
  in

  let rec contains e lst = match lst with [] -> false
  | h::t -> if h = e then true else contains e t in

  let rec getedgesleft acc gr= 
    match gr with [] -> acc 
    | (a,b,c)::t -> if contains a acc then getedgesleft acc t else let acc = a::acc in getedgesleft acc t in

  let rec getedgesright acc gr= 
      match gr with [] -> acc 
      | (a,b,c)::t -> if contains c acc then getedgesright acc t else let acc = c::acc in getedgesright acc t in

  let alledges = List.sort compare (getedgesleft (getedgesright [] graph) graph) in

  let rec brain acc nodes remgr = function 
  [] -> acc
  | h::t -> let l1,l2 = splitgraph h nodes remgr in brain (addedge(searchmin l1) acc) (h::nodes) l2 t in

  brain [] [List.hd alledges] graph (List.tl alledges) *)

let mst graph= 
  if List.length graph = 0 then []
  else 

  let rec contains e lst = match lst with [] -> false
  | h::t -> if h = e then true else contains e t in
  let rec getedgesleft acc gr= 
    match gr with [] -> acc 
    | (a,b,c)::t -> if contains a acc then getedgesleft acc t else let acc = a::acc in getedgesleft acc t in

  let rec getedgesright acc gr= 
      match gr with [] -> acc 
      | (a,b,c)::t -> if contains c acc then getedgesright acc t else let acc = c::acc in getedgesright acc t in

  let alledges = List.sort compare (getedgesleft (getedgesright [] graph) graph) in
  
  let uniquedges = List.sort_uniq (fun x y -> if x = y then 0 else if x < y then 1 else -1) alledges in
    
  let rec searchmin lst = 
        List.fold_left (fun x y -> let (a,b,c) = y in let (a1,b1,c1) = x in if b1 > b then y else x) (List.hd lst) (List.tl lst) in

  let rec usedNodes (a,c) lst = match lst with
        []->[a;c]
        |h::t-> if h = a then c::h::t else if h = c then a::h::t else h::usedNodes (a,c) t in
    
    
  let rec leftedges lst vertex acc=match lst with
        []->acc
      |h::t-> let (a,b,c) = h in if a=vertex || c = vertex then leftedges t vertex ((a,b,c)::acc) else leftedges t vertex acc in
    
    
  let rec getminedge lst acc nodes=match lst with
      |[]->acc 
      |(a,b,c)::t->if ((contains a nodes) && (contains c nodes)) then  getminedge t acc nodes else
            let (a1,b1,c1)=acc in if b1>b then getminedge t (a,b,c) nodes else getminedge t acc nodes in 
    
    
  let rec brain nodes lst = 
      let rec brain_helper nodes acc = 
        match nodes with
        |[]->acc
        |h::t-> let (a1,cost,c1) = acc in
            let (a,b,c) = getminedge (leftedges lst h []) (0,Float.max_float,0) nodes in
            if b < cost then brain_helper t (a,b,c) else brain_helper t acc in brain_helper nodes (0,Float.max_float,0) in 
    
    
  let rec helper ind acc tmp= if ind = 1 then tmp else
        let (a,b,c) = brain acc graph in helper (ind - 1) (usedNodes (a,c) acc) ((a,b,c)::tmp) in 
    helper (List.length uniquedges) [List.hd uniquedges] []


