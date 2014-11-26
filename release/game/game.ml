open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to yout own state type*)
type game = Game of game_status_data

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
    match color with 
    | Red -> failwith "todo" (* (None, ...) *)
    | Blue -> failwith "todo" (* (None, ...) *)
  in

  match g, ra, ba with 
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) ->
    (None, gsd, Some(Request(PickRequest(Red,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(Initialization.mon_table)))), None)
  | Game gsd, Action(PickSteammon name), DoNothing -> draft Red gsd name
  | Game gsd, DoNothing, Action(PickSteammon name) -> draft Blue gsd name  
  | _ -> failwith "swag"

 
let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
        TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(Initialization.mon_table))