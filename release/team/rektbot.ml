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

let firstTime = ref 0

let _ = Random.self_init ()
(*let calcScore (myMon:steammon) (tgtMon:steammon):int = (* returns multiplier *)
            List.fold_left (fun acc e -> 
              acc+int_of_float(snd(Util.calculate_type_matchup e 
              (myMon.first_type,myMon.second_type))))
            0 [tgtMon.first_move.element;tgtMon.second_move.element;
            tgtMon.third_move.element;tgtMon.fourth_move.element]*)

let rec findTheVeryBest (myMons:steammon list) (bestMon:steammon) 
(tgtMon:steammon):steammon=
  let myMonz = List.filter (fun a -> a.curr_hp > 0) myMons in
  match myMonz with
  | [] -> bestMon
  | h::t -> if Botutils.battleTurnout h tgtMon > 
  Botutils.battleTurnout bestMon tgtMon
            then findTheVeryBest t h tgtMon
            else findTheVeryBest t bestMon tgtMon

let rec findWeakest (myMons:steammon list) (oppMons:steammon list) 
(weakest:steammon):steammon=
  let myMonz = List.filter (fun a -> a.curr_hp > 0) myMons in
  match myMonz with
  | [] -> weakest
  | h::t -> let wk = List.fold_left (fun acc e -> 
  if Botutils.battleTurnout h e < 
  Botutils.battleTurnout weakest e then h else weakest) 
  weakest oppMons in findWeakest t oppMons wk
(*
let rec rater (mon:steammon) (pool:steammon list) 
        (acc:int*steammon):int*steammon =
          match pool with
          | [] -> acc
          | h::t -> rater mon t ((calcScore mon h)+fst(acc),mon)

let makePriorityList (pool:steammon list):steammon list=
          let rated = List.fold_left (fun acc e -> 
            (rater e pool (0,List.hd(pool)))::acc) [] pool in
          let sorted = List.sort (fun a b -> 
            compare (fst a) (fst b)) rated in
          List.fold_left (fun acc e -> 
            List.rev_append [snd(e)] acc) [] sorted

let types = [("Fire",1);("Water",2);("Ice",3);("Grass",4);
        ("Poison",5);("Normal",6);("Flying",7);("Psychic",8);("Ghost",9);
        ("Dark",10);("Steel",11);("Rock",12);("Ground",13);("Electric",14);
        ("Bug",15);("Dragon",16);("Fighting",17);("Typeless",18)]

let density = [("Fire",0);("Water",0);("Ice",0);("Grass",0);
        ("Poison",0);("Normal",0);("Flying",0);("Psychic",0);("Ghost",0);
        ("Dark",0);("Steel",0);("Rock",0);("Ground",0);("Electric",0);
        ("Bug",0);("Dragon",0);("Fighting",0);("Typeless",0);]

let rec typeCount (name:string) (pairLst:(string*int) list) 
        (count:int):int =
          match pairLst with
          | [] -> count
          | (str,n)::t when str = name -> typeCount name [] n
          | h::t -> typeCount name t count

let typeExtract (opt:steamtype option):steamtype =
  match opt with
  | Some x -> x
  | None -> Typeless

let rec mostCommon (myMons:steammon list) (acc:steamtype list)
        (max:int):steamtype list =
          match myMons with
          | [] -> acc
          | h::t -> let first = typeExtract h.first_type in
                    let second = typeExtract h.second_type in
                let count1 = typeCount 
            (Util.string_of_type first) density 0
                    and count2 = typeCount
            (Util.string_of_type second) density 0 in
            if max < count1
            then mostCommon t ((first)::acc) count1
            else if max < count2
            then mostCommon t ((second)::acc) count2
            else mostCommon t acc max

let rec isCommon (mon:steammon) (lst:steamtype list) 
        (tru:int):int =
          match lst with
          | [] -> tru
          | h::t -> if h = (typeExtract mon.first_type) || 
          h = (typeExtract mon.second_type)
                    then isCommon mon t tru+1
                    else isCommon mon t tru

let filterPool (comLst:steamtype list) (acc:steammon list) 
(pool:steammon list):steammon list =
          List.filter (fun e -> (isCommon e comLst 0) > 0) pool*)

let oppTeam = ref []

let getUpdatedTeam (current:steammon list):steammon list=
  oppTeam := current; current

let rec getNewMons (old:steammon list) (current:steammon list) 
(return:steammon list):steammon list =
  match current with 
  | [] -> return
  | h::t -> getNewMons old t (List.fold_left 
    (fun acc e -> if e.species <> h.species then e::acc else acc) 
  return old)

let getAvgCost (pool:steammon list):int=
  let total_cost = List.fold_left (fun acc e -> acc+e.cost) 0 pool in
  int_of_float(float_of_int (total_cost)/.
    float_of_int(List.length pool))

let isWithinCostRange (cost:int) (otherCost:int):bool = 
  abs(cost-otherCost) > 10

let firstPick (pool:steammon list):steammon =
  let darkghost = List.filter (fun a -> match a.first_type,a.second_type with
    | Some Dark, Some Ghost | Some Ghost, Some Dark -> true
    | _ -> false) pool in
  if List.length(darkghost) > 0 then
    List.hd(darkghost)
  else
    let groundflying = List.filter (fun a -> match a.first_type,a.second_type with
    | Some Ground, Some Flying | Some Flying, Some Ground -> true
    | _ -> false) pool in
  if List.length(groundflying) > 0 then
    List.hd(groundflying)
  else
    let darkflying = List.filter (fun a -> match a.first_type,a.second_type with
    | Some Dark, Some Flying | Some Flying, Some Dark -> true
    | _ -> false) pool in
  if List.length(darkflying) > 0 then
    List.hd(darkflying)
  else
    let evaluate (s:steammon):int = 
    let moves = [s.first_move;s.second_move;s.third_move;s.fourth_move] in
    List.fold_left (fun acc e -> acc+(int_of_float(float_of_int (e.power)*.
      float_of_int(e.accuracy)/.100.))) 0 moves
    in
    let sortedMons = Botutils.extractThing(List.sort Botutils.greaterStat 
    (Botutils.pairStat (fun mon -> evaluate mon) pool)) in
    let suitablyPriceMons = List.filter (fun a -> isWithinCostRange 
    (a.cost) (getAvgCost pool)) sortedMons in
    match suitablyPriceMons with
    | [] -> failwith "something wrong happened???"
    | h::t -> h
  

let minCostDiffMon (pool:steammon list) (theirMon:steammon):steammon=
  let head = List.hd pool in
  List.fold_left (fun acc e -> if abs(theirMon.cost-e.cost)<
    abs(theirMon.cost-acc.cost) then e else acc) head pool

let handleSingleDraft (s:steammon) (pool:steammon list):steammon=
  let sortedTuples = List.sort Botutils.greaterStat 
    (Botutils.pairStat (fun mon -> Botutils.battleTurnout mon s) pool) in
  let okMons = Botutils.extractThing (List.filter (fun a -> fst a > 0) 
    sortedTuples) in
  minCostDiffMon okMons s

(*let handleCoupleDraft (sl:steammon list) (pool:steammon list):steammon=
  match sl with
  | h::h2::t -> let first = List.fold_left 
  (fun acc e -> handleSingleDraft h pool) h sl in
  let second = List.fold_left (fun acc e -> handleSingleDraft h2 pool) 
  h2 t in
  firstPick [first;second]
  | _ -> failwith "wtf happened here"*)

(*let smartPick (theirNew:steammon list) (pool:steammon list):steammon=
  match theirNew with
  | h::t -> handleSingleDraft h pool
  | _ -> failwith "how did they not draft anything???"*)

let makeTheMostOf (pool:steammon list) (creds:int):steammon =
  List.fold_left (fun acc e -> if (creds-e.cost < creds-acc.cost) 
    && (e.cost<creds) then e else acc) (List.hd pool) pool

let tankPick c gsd pool = 
  let tanks = Botutils.extractThing (List.sort greaterStat 
    (pairStat (fun mon -> mon.defense+mon.max_hp+mon.spl_defense) pool)) in
  let pick = List.hd tanks in
    PickSteammon (pick.species)

let pickReq1 c gsd sp =
          let opp = if c = Red then snd gsd else fst gsd in
          let (monz,_,_) = opp in
          if List.length(monz) = 0 then (*you get first pick*)
          let pick = firstPick sp in
          PickSteammon (pick.species)
        else
          let otherTeam = if c = Red then snd gsd else fst gsd in
          let (mons,_,_) = otherTeam in
          oppTeam := mons;
          let myTeam = if c = Red then fst gsd else snd gsd in
          let (myMons,_,creds) = myTeam in
          if List.length (myMons) >= cNUM_PICKS-1 then
            (* last mon,draft the most expensive u can afford *)
            let pick = makeTheMostOf sp creds in
                PickSteammon (pick.species)
          else
            let newMons = getNewMons (!oppTeam) (getUpdatedTeam mons) [] in
            if List.length(newMons) = 0 then
            let pick = firstPick sp in
            oppTeam := (List.hd newMons)::(!oppTeam);
            PickSteammon (pick.species)
            else
            let pick = handleSingleDraft (List.hd newMons) sp in
            oppTeam := (List.hd newMons)::(!oppTeam);
            PickSteammon (pick.species)

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
        let pick = if c = Red 
        then findTheVeryBest mons1 (List.hd mons1) (List.hd mons2)
        else findTheVeryBest mons2 (List.hd mons2) (List.hd mons1)
        in SelectStarter (pick.species)
      else
        let pick = if c = Red
        then findWeakest mons1 mons2 (List.hd mons1)
        else findWeakest mons2 mons1 (List.hd mons2)
      in firstTime := 1; SelectStarter (pick.species)
    | PickRequest (c, gsd, _, sp) ->
          (*pickReq1 c gsd sp*)
          tankPick c gsd sp
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
