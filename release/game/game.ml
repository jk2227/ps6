open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to yout own state type*)
type game = Game of game_status_data

let draftRD = ref 1
let draftpool = Initialization.mon_table 

let game_datafication g = match g with 
  | Game x -> x 
	
let game_from_data game_data = Game (game_data)

let handle_step g ra ba = 
    (*
        check if it's in pool of avail steammons
        if it is check that you have enough creds
        if you do then add it to the respective player's
        list of steammon's and sent pickrequest to appropriate player
        if not choose the lowest cost or something... i don't 
        know the details well for that case
    *)
  let draft color ((rsl,ri,rcred),(bsl,bi,bcred)) name = 
    if Table.mem draftpool name && (List.length rsl < 6 ||
      List.length bsl <6) then 
      let s = Table.find draftpool name in 
      let sCred = s.cost in 
      let isOdd = (!draftRD mod 2 = 1) in 
      match color, isOdd with 
      | Red, true -> if sCred <= rcred then 
            add_update(UpdateSteammon (name,s.curr_hp,s.max_hp,Red));
            add_update(SetChosenSteammon (name));
            Table.remove draftpool name;
            draftRD := !draftRD + 1; 
            let gsd = (((s::rsl),ri,(rcred-sCred)), (bsl,bi,bcred)) in
            (None, gsd, None,
               Some(Request(PickRequest(Blue,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))))) 
      | Red, false -> if sCred <= rcred then 
            add_update(UpdateSteammon (name,s.curr_hp,s.max_hp,Red));
            add_update(SetChosenSteammon (name));
            Table.remove draftpool name;
            draftRD := !draftRD +1;
            let gsd = (((s::rsl),ri,(rcred-sCred)), (bsl,bi,bcred)) in
            (None, gsd,Some(Request(PickRequest(Red,gsd,
            hash_to_list (Initialization.move_table), 
            hash_to_list(draftpool)))), None) 
      | Blue, true -> if sCred <= bcred then 
            add_update(UpdateSteammon (name,s.curr_hp,s.max_hp,Blue));
            add_update(SetChosenSteammon (name));
            Table.remove draftpool name;
            draftRD := !draftRD +1;
            let gsd = ((rsl,ri,rcred), (s::bsl,bi,bcred-sCred)) in
            (None, gsd,Some(Request(PickRequest(Red,gsd,
            hash_to_list (Initialization.move_table), 
            hash_to_list(draftpool)))), None) 
      | Blue, false -> if sCred <= bcred then 
            add_update(UpdateSteammon (name,s.curr_hp,s.max_hp,Blue));
            add_update(SetChosenSteammon (name));
            Table.remove draftpool name;
            draftRD := !draftRD + 1; 
            let gsd = ((rsl,ri,rcred), (s::bsl,bi,bcred-sCred)) in
            (None, gsd, None,
               Some(Request(PickRequest(Blue,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))))) 
    else if (List.length rsl = 6 && List.length bsl = 6) then 
      let gsd = ((rsl,ri,rcred),(bsl,bi,bcred)) in 
      (None, gsd, Some(Request(PickInventoryRequest gsd)), 
      Some(Request(PickInventoryRequest gsd))) 
    else failwith "meh"
  in

  match g, ra, ba with 
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) ->
    add_update (InitGraphics (rName,bName));
    if Random.int 2 = 0 then 
    (None, gsd, Some(Request(PickRequest(Red,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool)))), None)
    else (None, gsd, None, Some(Request(PickRequest(Blue,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))))) 
  | Game gsd, Action(PickSteammon name), DoNothing -> draft Red gsd name
  | Game gsd, DoNothing, Action(PickSteammon name) -> draft Blue gsd name 
  | Game gsd, Action(PickInventory (invlst1)), 
    Action(PickInventory (invlst2))-> failwith "TODO"
  | _ -> failwith "swag"

 
let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
        TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))