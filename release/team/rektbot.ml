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
        let ((mons1,_,_),(mons2,_,_)) = gs in
        if !firstTime = 1 then
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
      else firstTime := 1;
        let rec findWeakest (myMons:steammon list) 
        (oppMons:steammon list) (weakest:steammon):steammon=
        match myMons with
        | [] -> weakest
        | h::t -> let wk = List.fold_left (fun acc e -> 
        if calcScore h e < calcScore weakest e then h else weakest) 
        weakest oppMons in findWeakest t oppMons wk
        in let pick = if c = Red
        then findWeakest mons1 mons2 (List.hd mons1)
        else findWeakest mons2 mons1 (List.hd mons2)
    | PickRequest(c, gsd, _, sp) ->
        let rec rater (mon:steammon) (pool:steammon list) 
        (acc:int*steammon):int*steammon =
          match pool with
          | [] -> acc
          | h::t -> rater mon t ((calcScore mon h)+acc,mon)
        in
        let makePriorityList (pool:steammon list):steammon list=
          let rated = List.fold_left (fun acc e -> 
            (rater e pool)::acc) [] pool in
          let sorted = List.sort (fun a b -> 
            compare fst(a) fst(b)) pool in
          let priorityList = List.fold_left (fun acc e -> 
            List.rev_append [snd(e)] acc) [] sorted
        in
        let types = [("Fire",1);("Water",2);("Ice",3);("Grass",4);
        ("Poison",5);("Normal",6);("Flying",7);("Psychic",8);("Ghost",9);
        ("Dark",10);("Steel",11);("Rock",12);("Ground",13);("Electric",14);
        ("Bug",15);("Dragon",16);("Fighting",17);("Typeless",18)] in
        let density = [("Fire",0);("Water",0);("Ice",0);("Grass",0);
        ("Poison",0);("Normal",0);("Flying",0);("Psychic",0);("Ghost",0);
        ("Dark",0);("Steel",0);("Rock",0);("Ground",0);("Electric",0);
        ("Bug",0);("Dragon",0);("Fighting",0);("Typeless",0);] in
        let rec typeCount (name:string) (pairLst:(string*int) list) 
        (count:int):int =
          match pairLst with
          | [] -> count
          | (str,n)::t when str = name -> typeCount name [] n
          | h::t -> typeCount name t count
        in
        let rec mostCommon (myMons:steammon list) (acc:steamtype list)
        (max:int):steamtype list =
          match myMons with
          | [] -> acc
          | h::t -> let count1 = typeCount 
            (Util.string_of_type h.first_type) density 0
                    and count2 = typeCount
            (Util.string_of_type h.second_type) density 0 in
            if max < count1
            then mostCommon t (h.first_type)::acc count1
            else if max < count2
            then mostCommon t (h.second_type)::acc count2
            else mostCommon t acc max
        in
        let rec isCommon (mon:steammon) (lst:steamtype list) 
        (tru:int):int =
          match lst with
          | [] -> tru
          | h::t -> if string_of_type h = string_of_type (mon.first_type) ||
                       string_of_type h = string_of_type (mon.second_type)
                    then isCommon mon t tru+1
                    else isCommon mon t tru
        in
        let filterPool (comLst:type list) (acc:steammon list):steammon list =
          List.filter (fun e -> (isCommon e comLst 0) > 0) sp
        in
        let (a1,b1) = gsd in
        let myteam = if c = Red then a1 else b1 in
        let (mons,invo,creds) = myteam in
        let pick = List.hd (filterPool (mostCommon mons [] 0) [])
        in PickSteammon (pick.species)
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
