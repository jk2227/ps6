open Team
open Definitions
open Constants
open Botutils

(* Attention Student:
 * Do not change the arguments of handle_request. However, feel free to change 
 * all of the inside and write many helper functions if you want to have a good bot.
 *)

(* Change this to the name of your bot. *)
let name = "rekt" 

let _ = Random.self_init ()

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    (* send team name *)
    | TeamNameRequest -> SendTeamName(name)
    (* starter pokemon or pokemon replacement. when starting calculate highest expected *)
    | StarterRequest(gs)->
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let pick = 
          try List.find(fun x -> x.curr_hp > 0) mons 
          with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
        (match sp with
         | h::t ->
            let length = List.length sp in
            let my_pick = List.nth sp (Random.int length) in
              PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let (my_team, their_team) = if c = Red then (a1, b1) else (b1, a1) in
        let (mons, pack, credits) = my_team in
        let (theirmons, _, _) = their_team in
        (match (mons, theirmons) with
        | (myActive::inactive, theirActive::theirInactive) ->
          let bestSmForJob = Botutils.bestSm mons theirActive in
            if (battleTurnout myActive theirActive) < 0
            then SwitchSteammon bestSmForJob.species
            else
            (*Todo, account for everyone is bad*)
              if (not (myActive = bestSmForJob) && battleTurnout myActive bestSmForJob - battleTurnout myActive theirActive > 2)
              then SwitchSteammon bestSmForJob.species
              else UseMove (Botutils.bestMv myActive theirActive).name
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
    (* Pick all revives, maxpot, fullheal, xatk, xspd, xdef, ether *)
	 | PickInventoryRequest (gr) -> 
      let (cash, items) = 
          List.fold_left (fun (cash, count) e -> (cash mod e, (cash / e)::count))
          (cINITIAL_CASH, []) [cCOST_REVIVE;cCOST_MAXPOTION;cCOST_FULLHEAL; 
                               cCOST_XATTACK;cCOST_XSPEED;cCOST_XDEFEND;cCOST_ETHER] in
      match items with
      | [itmEt; itmDf; itmSp; itmAt; itmFh; itmMp; itmRev] ->
          PickInventory([itmEt;itmMp;itmRev;itmFh;itmAt;itmDf;itmSp])
      | _ -> PickInventory([cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
                       cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
