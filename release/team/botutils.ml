open Team
open Definitions
open Constants

(* Recoil damage ignored, also struggle ignored, also boosts and effects ignored*)
let expectedRandomMultiplier = (100. +. float_of_int cMIN_DAMAGE_RANGE) /. 2.

(* given an attacker and a defender, calculate and return the product of
 * the multipliers of attacker's move on the defender, taking into
 * account attack, burn, stab, and expected random multipliers.*)
let netExptMult (attacker: steammon) (defender: steammon) (m: move) : float = 
  let stabMult =
    match (attacker.first_type, attacker.second_type) with
      | (Some x, Some y) -> if x = m.element || y = m.element then cSTAB_BONUS else 1.0
      | (Some x, _) | (_, Some x) -> if x = m.element then cSTAB_BONUS else 1.0
      | _ -> 1.0
  in
  let burnMult =
    match (attacker.status, Util.is_special m.element ) with
      | (Some Burned, false) -> cBURN_WEAKNESS
      | _ -> 1.0
  in
  let (attEffectiveness, attMultiplier ) =
    Util.calculate_type_matchup m.element (defender.first_type,defender.second_type)
  in
  attMultiplier *. stabMult *. burnMult *. expectedRandomMultiplier

(* calculate the final damage steammon defender takes from steammon
 * attacker's move m. *)
let finalDmg (defender: steammon) (attacker: steammon) (m: move): int = 
  let (a,d) = (attacker, defender) in
  let (aAtk, aSpAtk, dDef, dSpDef) = 
    (int_of_float (Util.multiplier_of_modifier a.mods.attack_mod *. float_of_int a.attack),
     int_of_float (Util.multiplier_of_modifier a.mods.spl_attack_mod *. float_of_int a.spl_attack), 
     int_of_float (Util.multiplier_of_modifier d.mods.defense_mod *. float_of_int d.defense),
     int_of_float (Util.multiplier_of_modifier d.mods.spl_defense_mod *. float_of_int d.spl_defense))
  in
  match (m.pp_remaining, Util.is_special m.element) with 
    | (0, _) -> 0
    | (_, false) -> Util.calculate_damage aAtk dDef m.power (netExptMult a d m)
    | (_, true) -> Util.calculate_damage aSpAtk dSpDef m.power (netExptMult a d m)

(* calculate how many turns the defender steammon will stay alive
 * if attacked each turn by any of the attacker's moves. *)
let turnsToLive (defender: steammon) (attacker : steammon) : int =
  let attackermoves =
    [attacker.first_move; attacker.second_move; attacker.third_move; attacker.fourth_move]
  in
  let maxDPT = 
    List.fold_left max 0 (List.map (finalDmg defender attacker) attackermoves)
  in 1 + if maxDPT = 0 then 99999 else defender.curr_hp / maxDPT

(* calculate how well steammon s1 fares against steammon s2 in battle
 * and return an int. if the int > 0 then s1 is predicted to win, and
 * if the int < 0 then s2 is predicted to win, and there would be a tie
 * if the int = 0. *)
let battleTurnout (s1: steammon) (s2: steammon) : int = 
  let (s1Life,s2Life) = (turnsToLive s1 s2, turnsToLive s2 s1) in
  if s1Life = s2Life then
    if s1.speed = s2.speed then 0 else
    if s1.speed < s2.speed then -1 else 1 
  else
   s1Life - s2Life

(* returns the steammon revived *)
let postRevive (s: steammon) = 
  {s with curr_hp = s.max_hp/2}

(* given tuples of how effective a steammon is (int) and the actual
 * steammon, return an int > 0 if one is more effective or has better
 * stats than two, etc. *)
let greaterStat (one : int * 'a) (two : int * 'a) =
  let ((s1,sm1),(s2,sm2)) = (one, two) in s1 - s2

(* given a function that takes a steammon and returns some sort of 
 * stat that is expressed in an integer, and a list of steammon,
 * and returns a list of tuples with a steammon's effectiveness and the
 * steammon itself. *)
let pairStat (statFunc : 'a -> int) (smlst: 'a list) = 
  List.map (fun x -> (statFunc x, x)) smlst

(* handy function for extracting the steammons out from a list that's
 * been created by pairStat *)
let extractThing (lst : (int * 'a) list) =
  List.map (fun (bt, sm) -> sm) lst

(* looks through attacker's moves and determines the move that inflicts
 * the most damage against defender. returns tuple of damage attacker
 * can do with the most damaging move. *)
let bestMvWithDmg (attacker: steammon) (defender: steammon) = 
  let mvlst =
    [attacker.first_move; attacker.second_move; attacker.third_move; 
    attacker.fourth_move]
  in
  let orderedByDmg = 
    List.sort greaterStat (pairStat (fun m -> finalDmg defender 
    attacker m) mvlst)
  in
  match List.filter (fun (dmg, m) -> m.pp_remaining > 0) 
  orderedByDmg with
  | h::t -> h
  | _ -> (0, attacker.first_move)

(* returns the best move using bestMvWithDmg *)
let bestMv x y = snd (bestMvWithDmg x y)

(* given a function that takes a steammon and returns some form of
 * stat reflecting that steammon, return a list of steammon in 
 * descending order of their stat. *)
let orderDescBy (smlst: steammon list) (s: steammon) (f: steammon -> int) : steammon list =
  extractThing (List.sort greaterStat (pairStat f smlst))

(* returns a stat function *)
let genSteammonOrderBy (slst: steammon list) (s: steammon) =
  orderDescBy slst s

(* sorts the given steammon list in order of ascending battle
 * effectiveness against steammon s. *)
let bestSmOrder (smlst: steammon list) (s: steammon) : steammon list =
  extractThing (List.sort greaterStat (pairStat (fun x -> battleTurnout x s) smlst))

(* returns the most effective steammon against a steammon s. *)
let bestSm (smlst: steammon list) (s: steammon) : steammon = 
  match bestSmOrder smlst s with
  | h::t -> h
  | _ -> failwith "Something bad happened"

(*  *)
let mostSurvSmWithShiftAndDeath (smlst: (int*steammon*bool) list) (s: steammon) : (int*steammon*bool) list= 
  extractThing (List.sort greaterStat (pairStat (fun (a,b,_) -> a + turnsToLive b s) smlst))

(* *)
let mostSurvSm (smlst: steammon list) (s: steammon) : steammon list = 
  List.map (fun (_,x,_) -> x) (mostSurvSmWithShiftAndDeath (List.map (fun x -> (0, x, true)) smlst) s)

(* *)
let fastestKiller (smlst: steammon list) (s: steammon) : steammon list = 
  extractThing (List.sort greaterStat (pairStat (fun x -> fst (bestMvWithDmg x s)) smlst))

(* return the first living steammon in the list *)
let rec firstLiving (smlst: steammon list) = 
  match smlst with 
  | h::t -> if (h.curr_hp > 0) then h else firstLiving t
  | [] -> failwith "this should really never happen"

(* count how many steammon in a given list are alive *)
let numLiving (smlst: steammon list) =
  List.fold_left (fun a e -> if e.curr_hp > 0 then a+1 else a) 0 smlst

(*let pickingRanker (centralCost: int) : steammon -> steammon -> int = 
  fun me them ->
    let costDiff = abs (sm.cost - centralCost) in 
    let battleTurnout*)
(* BOT STRATEGIES *)
(* PICK STEAMMON THAT CAN SURVIVE MOST MINDLESS PUMMELING *)
let survivableMK1 (meActive:steammon) (meReserve:steammon list) (them:steammon) (inv:inventory) = 
  let hasRevive = match inv with 
    | _::_::r::_ -> r > 0
    | _ -> false
  in 
  let monList = match hasRevive with
    | true -> List.map (fun sm -> if sm.curr_hp = 0 
                                  then (-3,{sm with curr_hp = sm.max_hp / 2},true)
                                  else (-2,sm, false)) meReserve
    | false -> List.map (fun x -> (-2, x, false)) (List.filter (fun sm -> sm.curr_hp > 0) meReserve)
  in
  let rankedBySurv = mostSurvSmWithShiftAndDeath ((0,meActive,false)::monList) them in
  match rankedBySurv with 
  | (_,h2,h3)::t -> if (h2 = meActive) then UseMove (bestMv meActive them).name
                      else if h3 then UseItem (Revive, h2.species)
                      else SwitchSteammon h2.species
  | [] -> failwith "impossible case encountered"
