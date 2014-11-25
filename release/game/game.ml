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

let handle_step g ra ba =
	failwith 
    "Remember my super cool Rattata? My Rattata is different from regular 
    Rattata. It’s like my Rattata is in the top percentage of all Rattata."

                        (* Youngster Joey, about his Raticate *)

let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    ((([],[],0),([],[],0)),TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(Initialization.mon_table))