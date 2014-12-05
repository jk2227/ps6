open Team
open Definitions
open Constants
open Botutils

(** list ref to keep track of what was and what is currently on
 * the opponent's team; used to "spy" on what mons they drafted *)
let oppTeam = ref []

(** update the opponent's team to what's currently on their team
 * and return the opponent's current team *)
let getUpdatedTeam (current:steammon list):steammon list=
  oppTeam := current; current

(** given the opponent's steammon list from the last turn and
 * the opponent's steammon list from the current turn, return a
 * list of steammon added in the time between the last and 
 * current turn *)
let rec getNewMons (old:steammon list) (current:steammon list) 
(return:steammon list):steammon list =
  match current with 
  | [] -> return
  | h::t -> getNewMons old t (List.fold_left 
    (fun acc e -> if e.species <> h.species then e::acc else acc) 
  return old)

(** from a pool of steammon, determine the avg price of a mon *)
let getAvgCost (pool:steammon list):int=
  let total_cost = List.fold_left (fun acc e -> acc+e.cost) 0 pool in
  int_of_float(float_of_int (total_cost)/.
    float_of_int(List.length pool))

(** checks to make sure that 2 costs are at least 10 credits apart *)
let isWithinCostRange (cost:int) (otherCost:int):bool = 
  abs(cost-otherCost) <= 10

(** when our bot gets first pick or sees no more steammon on the
 * opponent's team to counter, then we find a preferrable steammon
 * with priority given to dark/ghost, ground/flying, dark/flying mons
 * because we believe that these are highly/relatively resistent types *)
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
  
(** given a list of steammons, finds the steammon that is closest in
 * price to the steammon theirMon *)
let minCostDiffMon (pool:steammon list) (theirMon:steammon):steammon=
  let head = List.hd pool in
  List.fold_left (fun acc e -> if abs(theirMon.cost-e.cost)<
    abs(theirMon.cost-acc.cost) then e else acc) head pool

(** find a steammon that has a positive battleTurnout against a certain
 * steammon s, and that is as close to the cost of the target steammon
 * s as possible *)
let handleSingleDraft (s:steammon) (pool:steammon list):steammon=
  let sortedTuples = List.sort Botutils.greaterStat 
    (Botutils.pairStat (fun mon -> Botutils.battleTurnout mon s) pool) in
  let okMons = Botutils.extractThing (List.filter (fun a -> fst a > 0) 
    sortedTuples) in
  minCostDiffMon okMons s

(** finds the most expensive steammon we can afford with creds number
 * of credits left to spend *)
let makeTheMostOf (pool:steammon list) (creds:int):steammon =
  let filtered = List.filter (fun a -> a.cost <= creds) pool in
  List.fold_left (fun acc e -> if (creds-e.cost < creds-acc.cost) 
    && (e.cost<creds) then e else acc) (List.hd filtered) filtered

(*let tankPick c gsd pool = 
  let ((_,_,cr),(_,_,cb)) = gsd in
  let rc = if c = Red then cr else cb in 
  let tanks = Botutils.orderDescBy pool 
    (fun s -> s.max_hp*s.defense*s.spl_defense) in
  let potatoes = List.filter 
  (fun a -> a.cost <= min rc cSTEAMMON_CREDITS/(cNUM_PICKS-2)) tanks in
  let pick = List.hd potatoes in
    PickSteammon (pick.species)*)

(*let strikerPick c gsd pool =
  let strikers = Botutils.orderDescBy pool
    (fun s -> s.attack*s.spl_attack*s.speed) in
  let asparagus = List.filter
  ()*)

(** handles our bot's response to a PickRequest in drafting.
 * when our bot gets first pick from the pool or needs to pick
 * a mon to counter one of the new mons drafted by the opponent,
 * it picks the most preferred mon from the pool.
 * if it is time for our bot to draft its last steammon, we pick
 * the most expensive steammon we can afford with our remaining credits *)
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
            (* last mon, draft the most expensive you can afford *)
            let pick = makeTheMostOf sp creds in
                PickSteammon (pick.species)
          else
            let newMons = getNewMons (!oppTeam) (getUpdatedTeam mons) [] in
            if List.length(newMons) = 0 then
            let pick = firstPick sp in
            PickSteammon (pick.species)
            else
            let pick = handleSingleDraft (List.hd newMons) sp in
            oppTeam := (List.hd newMons)::(!oppTeam);
            PickSteammon (pick.species)

(** handles our bot's response to StarterRequest *)
let startReq c gs =
  let (mons1,_,_),(mons2,_,_) = gs in
    if c = Red
    then let pick = List.fold_left 
    (fun acc e -> Botutils.bestSm mons1 (List.hd mons2)) 
    (List.hd mons1) mons1 in
      SelectStarter (pick.species)
    else let pick = List.fold_left 
    (fun acc e -> Botutils.bestSm mons2 (List.hd mons1)) 
    (List.hd mons2) mons2 in
      SelectStarter (pick.species)