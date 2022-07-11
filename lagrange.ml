let lagrange lst find = 
  let rec pi lst jx = 
    List.fold_left (fun x y -> let(a,b) = y in if jx = a then x else ((find -.a) /. (jx-.a))*.x ) 1. lst in
  let rec sigm lst find =
    List.fold_left (fun x y -> let (a,b) = y in (pi lst a) *. b +. x) 0. lst in
  sigm lst find
 