open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to yout own state type*)
type game = Game of game_status_data

let game_datafication g = match g with 
  | Game x -> x 
  | _ -> failwith "bleh"
	
let game_from_data game_data = Game (game_data)

  let battle gsd rs bs = 
   (*I think faster one attacks first*) 
   (* add_update (SetFirstAttacker Red) ; *)
    failwith "TODO"
  in 

  let exception_handle gsd = 
    if !init = 1 then 
        initialize gsd "Red" "Blue"
    else if !drafting = 1 then 
      match (hash_to_list draftpool) with 
      | h::t -> let (n,m) = findMin h.species h in
        draftupdate n m !draftColor;
        let ((rsl,ri,rcred),(bsl,bi,bcred)) = gsd in begin
        match !draftColor with 
        | Red -> changeColor ();  
                 if m.cost > rcred then 
                   let gsd = (((m::rsl),ri,0), (bsl,bi,bcred)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(!draftColor,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool))))) 
                else  
                  let gsd = (((m::rsl),ri,(rcred-m.cost)), (bsl,bi,bcred)) in
                  (None, gsd, None, 
                  Some(Request(PickRequest(Blue,gsd, 
                  hash_to_list (Initialization.move_table), 
                  hash_to_list(draftpool))))) 
        | Blue -> changeColor ();
                  if m.cost > bcred then 
                    let gsd = ((rsl,ri,rcred), (m::bsl,bi,0)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(Blue,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool)))))
                  else 
                    let gsd = ((rsl,ri,(rcred)), (m::bsl,bi,bcred-m.cost)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(Blue,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool)))))
        end
      | _ -> failwith "not enough steammons available to complete draft" 
    else if !invent = 1 then 
      inventory gsd [] [] true
    else failwith "Finish exception handling from Section 4.6"
  in 

let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    ((([],[],0),([],[],0)),TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(Initialization.mon_table))
