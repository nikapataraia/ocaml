#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;
open Thread
open Event

let spawn_counter a =
  let rec helper x = 
    (print_int Thread.(id(self()));print_string "-";print_int x ;print_string "\n";);
    if x < a then (helper (x+1) )else () in
  Thread.create helper 0;;

let run_counters a b =
 let rec helper co = if co > a then () else
 (Thread.join (spawn_counter b); helper (co +1)) in
 helper 0


let new_spawn_counter a inchan outchan b =
    let rec helper (x,b1)= if b1 then (print_int Thread.(id(self()));print_string "-";print_int x ;print_string "\n"; sync (send outchan true)) else
      (let _ = sync (receive inchan) in print_int Thread.(id(self()));print_string "-";print_int x ;print_string "\n"; sync (send outchan true));
      if x < a then (helper ((x+1),false)) else (if b then (let _ =  sync(receive inchan) in ()) else ()) in
    Thread.create helper (0,b);;
  
let new_run_counters a b = 
  let rec helper2 co lst = match co with 0 -> lst | _-> helper2 (co-1) ((new_channel ())::lst) in
  let chans = helper2 a [] in
  let rec helper co lst = if co = a then   lst
    else helper (co+1) ((new_spawn_counter b (List.nth chans co) (List.nth chans (if co = (a-1) then 0 else (co+1))) (if co=0 then true else false))::lst) in

  let mylst = helper 0 [] in Thread.join (List.nth mylst (0));;
  (* let rec joinall lst =
   match lst with [] -> () | a::b -> (Thread.join a); joinall b in
  joinall mylst *)
  (* Thread.join (List.nth mylst (a-1)) *)