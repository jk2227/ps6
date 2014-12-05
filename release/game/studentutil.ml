open Definitions
open Util
open Constants
open Netgraphics
open Initialization

let dummymove = {name=" "; element=Typeless; target=User; max_pp=0; 
  pp_remaining=0; power=0; accuracy=0;effects=None}
let no_mods = {attack_mod=0; defense_mod=0; spl_attack_mod=0; 
  spl_defense_mod=0; speed_mod=0}
let dummymon = {species="dummy"; curr_hp=0; max_hp=0; first_type=None; 
  second_type=None; first_move=dummymove; second_move=dummymove; 
  third_move=dummymove; fourth_move=dummymove; attack=0; spl_attack=0; 
  defense=0; spl_defense=0; speed=0; status=None; mods=no_mods; cost=0}

 (*Finds a steammon with the given name in the given list and returns it. 
    Returns None if the steammon was not found. *)
  let rec get_mon lst name : steammon option =
    match lst with
    | [] -> None
    | h::t -> if h.species = name then Some h else get_mon t name

(*returns the position of the given item in the inventory list.
  Ether is 1, MaxPotion is 2, etc. *)
let item_index item =
  match item with
  | Ether -> 1
  | MaxPotion -> 2
  | Revive -> 3
  | FullHeal -> 4
  | XAttack -> 5
  | XDefense -> 6
  | XSpeed -> 7

(*returns the number of copies of the item in the inventory*)
let num_items_left item inv =
  (*returns the nth element of the given list*)
  let rec nth_entry lst n =
    match lst with 
    | [] -> 0 (*should not be reached for the purposes of PokeJouki*)
    | h::t -> if n = 1 then h else nth_entry lst (n - 1) in
  nth_entry inv (item_index item)

(*returns true if the given item can be used for the given steammon and team.
  false otherwise.*)
let valid_item_use item s_name sl inv =    
  (*let rec mon_present lst s_name =
    match lst with
    | [] -> false
    | h::t -> (h.species = s_name) || (mon_present t s) in*)
  if num_items_left item inv < 1 then false else
  match get_mon sl s_name , item with
  | None , _ -> false
  | Some mon , Revive -> mon.curr_hp = 0
  | Some mon, _ -> mon.curr_hp > 0

(*returns an inventory with one copy of the given item taken away from the
  given inventory. if the item is already at 0 copies, then it stays 
  at 0. *)
let consume_item item inv : inventory =
  let rec replace_el e lst i n =
    match lst with 
    | [] -> []
    | h::t -> if i = n then e::t else h::(replace_el e t (i+1) n) in
  let amount = num_items_left item inv in
  if amount = 0 then inv 
  else replace_el (amount - 1) inv 1 (item_index item)

(*returns the modifier multiplier of the steammon for the given stat*)
let modifier mon stt : float = 
  let stage = match stt with
  | Atk -> mon.mods.attack_mod
  | Def -> mon.mods.defense_mod
  | SpA -> mon.mods.spl_attack_mod
  | SpD -> mon.mods.spl_defense_mod
  | Spe -> mon.mods.speed_mod in
  multiplier_of_modifier stage

(*takes two steammon and determines which color should perform its action
  first. the first steammon is team Red and the second steammon is team
  Blue. *)
let first_action rs bs : color =
  let speed s = 
    if s.status = Some Paralyzed then 
      int_of_float( (float_of_int s.speed) *. 
        (multiplier_of_modifier s.mods.speed_mod) /. 4. )
    else
      int_of_float( (float_of_int s.speed) *. 
        (multiplier_of_modifier s.mods.speed_mod) ) in 
  let rspeed = speed rs
  and bspeed = speed bs in
  if rspeed > bspeed then Red 
  else if bspeed > rspeed then Blue 
  else if Random.int 2 = 0 then Red 
  else Blue

(*returns true at a certain probability. true means that the move hits 
and false means that the move misses.*)
let move_hit m : bool =
  if m.power = 0 && m.target = User then true
  else if (Random.int 100 + 1) < m.accuracy then true
  else false

(*determines how much damage the move deals based on the given user
  and target pair. *)
let move_dmg m user_mon target_mon : int = 
  if m.power = 0 then 0 else
  let stab = 
    match user_mon.first_type, user_mon.second_type with 
    | None, None -> 1.0
    | Some t, None -> if t = m.element then cSTAB_BONUS else 1.0
    | None, Some u -> if u = m.element then cSTAB_BONUS else 1.0
    | Some t, Some u -> if t = m.element || u = m.element then cSTAB_BONUS 
                         else 1.0
  and typemult = 
    snd (calculate_type_matchup m.element (target_mon.first_type, 
      target_mon.second_type))
  and burn =
    match user_mon.status with
    | Some Burned -> if is_special m.element then 1.0 else cBURN_WEAKNESS
    | Some _ -> 1.0
    | None -> 1.0 
  and rand = (Random.int (100 - cMIN_DAMAGE_RANGE + 1)) + cMIN_DAMAGE_RANGE in
  let multiplier = stab*.typemult*.burn*.(float_of_int rand)/.(100.0) in

  let atk = if is_special m.element then 
    int_of_float( (float_of_int user_mon.spl_attack)*.(modifier user_mon SpA)) 
  else
    int_of_float( (float_of_int user_mon.attack)*.(modifier user_mon Atk) ) in

  let def = if is_special m.element then
    int_of_float( (float_of_int target_mon.spl_defense) *.
      (modifier target_mon SpD))
  else
    int_of_float( (float_of_int target_mon.defense) *.
      (modifier target_mon Def)) in

  (calculate_damage atk def m.power multiplier)

(*replaces the move with the same name as the given move in the steammon.
  the altered steammon is returned. if the steammon does not know the move,
  then the steammon is returned unchanged.*)
let replace_move mon (m: move) : steammon =
  let name = m.name in
  if mon.first_move.name = name then begin 
    {mon with first_move = m} 
  end
  else if mon.second_move.name = name then {mon with second_move = m}
  else if mon.third_move.name = name then {mon with third_move = m}
  else if mon.fourth_move.name = name then {mon with fourth_move = m}
       else mon

(*returns true if either of the steammon have fainted. false otherwise*)
let check_faint (user, opp) : bool =
  user.curr_hp <= 0 || opp.curr_hp <= 0

(*returns true if all the steammon in the list have fainted. false otherwise.
  note: if the empty list is given, then true is returned from the 
  trivial case. *)
let rec all_fainted sl : bool =
  match sl with
  | [] -> true
  | h::t -> h.curr_hp = 0 && all_fainted t



