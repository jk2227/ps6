open Team
open Definitions
open Constants

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "recbot" 

let _ = Random.self_init ()

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)
    | StarterRequest(gs)-> (* always pick based on opponent's first 
        mon, regardless of which turn this is *)
        let ((mons1,_,_),(mons2,_,_)) = gs in
          let calcScore (myMon:steammon) (tgtMon:steammon):int =
            List.fold_left (fun acc e -> 
              match Util.calculate_type_matchup e 
              (tgtMon.first_type,tgtMon.second_type) with
              | (Regular,_) -> acc + 2
              | (SuperEffective,_) -> acc + 4
              | _ -> acc - 1)
            0 [myMon.first_move.element;myMon.second_move.element;
            myMon.third_move.element;myMon.fourth_move.element]
          in
          let rec findTheVeryBest (myMons:steammon list) (bestMon:steammon) 
          (tgtMon:steammon):steammon=
            match myMons with
            | [] -> bestMon
            | h::t -> if calcScore h tgtMon > calcScore bestMon tgtMon
                      then findTheVeryBest t h tgtMon
                      else findTheVeryBest t bestMon tgtMon
        in let pick = if c = Red 
        then findTheVeryBest mons1 (List.hd mons1) (List.hd mons2)
        else findTheVeryBest mons2 (List.hd mons2) (List.hd mons1)
        in SelectStarter (pick.species)
    | PickRequest(_, _, _, sp) ->
        (match sp with
         | h::t ->
            let length = List.length sp in
            let my_pick = List.nth sp (Random.int length) in
              PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        (match mons with
        | h::t ->
            if (h.first_move).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_move).name)) in
                UseMove((h.first_move).name)
            else if ((h.second_move).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_move).name)) in
                UseMove((h.second_move).name)
            else if ((h.third_move).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_move).name)) in
                UseMove((h.third_move).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_move).name)) in
                UseMove((h.fourth_move).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
	 | PickInventoryRequest (gr) -> PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
