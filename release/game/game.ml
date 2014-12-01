open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to yout own state type*)
type game = Game of game_status_data
let init = ref 1
let drafting = ref 0
let invent = ref 0
let battling = ref 0 
let draftRD = ref 1
let draftColor = ref Red 
let draftpool = Initialization.mon_table 
let movepool = Initialization.move_table

let game_datafication g = match g with 
  | Game x -> x 
  
let game_from_data game_data = Game (game_data)

let handle_step g ra ba = 
  
  let changeColor () = 
    if !draftRD mod 2 = 1 then draftColor := invert_color !draftColor
  in 

  let draftupdate name s c=
    add_update(UpdateSteammon (name, s.curr_hp, s.max_hp, c)); 
    Table.remove draftpool name; 
    draftRD := !draftRD + 1; ()
  in

  let findMin name s = 
    Table.fold (fun k v (n,m) -> 
        if v.cost < m.cost then (k,v) else (n,m))
              draftpool (name,s) 

   in
  
  let draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) c s z = 
    match c, z with 
    | Red, true -> (((s::rsl),ri,0), (bsl,bi,bcred))
    | Red, false -> (((s::rsl),ri,(rcred-s.cost)), (bsl,bi,bcred))   
    | Blue, true -> ((rsl,ri,rcred), (s::bsl,bi,0))
    | Blue, false -> ((rsl,ri,rcred),(s::bsl,bi,bcred-s.cost))
  in

  let draft ((rsl,ri,rcred),(bsl,bi,bcred)) name = 
    if ((List.length rsl = cNUM_PICKS) && (List.length bsl = cNUM_PICKS)) then
      begin 
      drafting := 0;
      invent := 1; 
      add_update(Message "End of draft"); 
      let gsd = ((rsl,ri,rcred),(bsl,bi,bcred)) in 
        (None, gsd, Some(Request(PickInventoryRequest gsd)), 
          Some(Request(PickInventoryRequest gsd))) 
      end 

    else 
      let s = Table.find draftpool name in
      match !draftColor with 
      | Red when List.length rsl < cNUM_PICKS -> begin
          changeColor ();
          if s.cost < rcred then begin 
            draftupdate name s Red; 
            let gsd = draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) Red s false in
            (None, gsd, None, 
              Some(Request(PickRequest(!draftColor,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))))
          end
          else begin
            let (min_name, min_mon) = findMin name s in 
            draftupdate min_name min_mon Red; 
            let gsd = draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) Red min_mon 
            (min_mon.cost > rcred) in
            (None, gsd, None, Some(Request(PickRequest(!draftColor,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))))   
          end  
        end
      | Blue when List.length bsl < cNUM_PICKS -> begin
          changeColor ();
          if s.cost < rcred then begin 
            draftupdate name s Blue; 
            let gsd = draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) Blue s false in
            (None, gsd, None, 
              Some(Request(PickRequest(!draftColor,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))))
          end
          else begin
            let (min_name, min_mon) = findMin name s in 
            draftupdate min_name min_mon Blue; 
            let gsd = draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) Blue min_mon 
            (min_mon.cost > rcred)in
            (None, gsd, None, Some(Request(PickRequest(!draftColor,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))))   
          end  
        end
       | _ -> failwith "should never reach here"
      in

  let inventory ((rsl,ri,rcred),(bsl,bi,bcred)) invlstR invlstB exc = begin
    invent := 0; 
    battling := 1; 
    let default = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE;
     cNUM_FULL_HEAL; cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in 
    let itemlst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_REVIVE;
    cCOST_FULLHEAL; cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in 
    if exc then 
      let gsd = ((rsl,default,rcred),(bsl,default,bcred)) in 
      (None, gsd, Some(Request(StarterRequest(gsd))), 
        Some(Request(StarterRequest(gsd))))
    else 
      let rcost = List.fold_left2 (fun acc a b-> acc+(a*b)) 0 invlstR itemlst in 
      let bcost = List.fold_left2 (fun acc a b-> acc+(a*b)) 0 invlstB itemlst in 
      match (rcost > cINITIAL_CASH), (bcost > cINITIAL_CASH) with 
      | true, true -> let gsd = ((rsl,default,rcred),(bsl,default,bcred)) in 
      (None, gsd, Some(Request(StarterRequest(gsd))), 
        Some(Request(StarterRequest(gsd))))
      | true, false -> let gsd = ((rsl,default,rcred),(bsl,invlstB,bcred)) in 
      (None, gsd, Some(Request(StarterRequest(gsd))), 
        Some(Request(StarterRequest(gsd))))
      | false, true -> let gsd = ((rsl,invlstR,rcred),(bsl,default,bcred)) in 
      (None, gsd, Some(Request(StarterRequest(gsd))), 
        Some(Request(StarterRequest(gsd))))
      | false, false -> let gsd = ((rsl,invlstR,rcred),(bsl,invlstB,bcred)) in 
      (None, gsd, Some(Request(StarterRequest(gsd))), 
        Some(Request(StarterRequest(gsd))))
  end in 

  let initialize gsd red blue = begin
    send_update (InitGraphics (red,blue));
    init := 0; 
    drafting := 1;
    Random.self_init () ; 
    if (Random.int 2) = 0 then begin
      draftColor := Red; 
    (None, gsd, Some(Request(PickRequest(Red,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool)))), None)
    end
    
    else begin 
      draftColor := Blue; 
      (None, gsd, None, Some(Request(PickRequest(Blue,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool)))))
    end

  end in

  let battle gsd rs bs = 
   (*I think faster one attacks first*) 
   (* add_update (SetFirstAttacker Red) ; *)
    failwith "TODO"

  in 

  let exception_handle gsd = 
    if !init = 1 then 
        initialize gsd "Red" "Blue"
    else if !drafting = 1 then 
      match (hash_to_list draftpool) with 
      | h::t -> let (n,m) = findMin h.species h in
        draftupdate n m !draftColor;
        let ((rsl,ri,rcred),(bsl,bi,bcred)) = gsd in begin
        match !draftColor with 
        | Red -> changeColor ();  
                 if m.cost > rcred then 
                   let gsd = (((m::rsl),ri,0), (bsl,bi,bcred)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(!draftColor,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool))))) 
                else  
                  let gsd = (((m::rsl),ri,(rcred-m.cost)), (bsl,bi,bcred)) in
                  (None, gsd, None, 
                  Some(Request(PickRequest(Blue,gsd, 
                  hash_to_list (Initialization.move_table), 
                  hash_to_list(draftpool))))) 
        | Blue -> changeColor ();
                  if m.cost > bcred then 
                    let gsd = ((rsl,ri,rcred), (m::bsl,bi,0)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(Blue,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool)))))
                  else 
                    let gsd = ((rsl,ri,(rcred)), (m::bsl,bi,bcred-m.cost)) in
                    (None, gsd, None, 
                    Some(Request(PickRequest(Blue,gsd, 
                    hash_to_list (Initialization.move_table), 
                    hash_to_list(draftpool)))))
        end
      | _ -> failwith "not enough steammons available to complete draft" 
    else if !invent = 1 then 
      inventory gsd [] [] true
    else failwith "Finish exception handling from Section 4.6"
  in 

  

  (*takes a team_data and a steammon. returns a team_data with the given
    steammon placed at the head of the steammon list (as the active pokemon) 
    the pokemon switched out has its modifiers removed. *)
  let switch_in (sl,i,cred) s =
    let no_mods = {attack_mod=0; defense_mod=0; spl_attack_mod=0; spl_defense_mod=0; speed_mod=0} in
    let active_mon = List.hd sl in
    let sl' = {active_mon with mods = no_mods}::(List.tl sl) in
    (*returns the given list with the given mon removed. preserves order.*)
    let rec remove_mon lst mon = 
      match lst with
      | [] -> []
      | h::t -> if h.species = mon.species then t else h::(remove_mon t mon) in
    (remove_mon sl' s, i, cred) in

  (*uses the given item on the given steammon and returns the altered 
    steammon. 'active' is a boolean that is true if the given steammon
    is currently an active steammon and false otherwise. assumes that this item
    usage does not violate any rules. *)
  let item_on_mon item mon active : steammon = 

    (*applies ether to a single move. returns the move with additional pp.*)
    let ether move = 
      let new_pp = if (move.max_pp - move.pp_remaining) < 5 then move.max_pp else move.pp_remaining + 5 in
      {move with pp_remaining = new_pp} in

    match item with
    | Ether -> { mon with first_move = ether mon.first_move;
                          second_move = ether mon.second_move;
                          third_move = ether mon.third_move;
                          fourth_move = ether mon.fourth_move
               }
    | MaxPotion -> {mon with curr_hp = mon.max_hp}
    | Revive -> {mon with curr_hp = mon.max_hp/2 }
    | FullHeal -> {mon with status = None}
    | XAttack when active -> 
      let mods' = {mon.mods with attack_mod = mon.mods.attack_mod + 1} in
      {mon with mods = mods'}
    | XDefense when active ->
      let mods' = {mon.mods with defense_mod = mon.mods.defense_mod + 1} in
      {mon with mods = mods'}
    | XSpeed when active ->
      let mods' = {mon.mods with speed_mod = mon.mods.speed_mod + 1} in
      {mon with mods = mods'}
    | _ -> mon in

  let item_index item =
    match item with
    | Ether -> 1
    | MaxPotion -> 2
    | Revive -> 3
    | FullHeal -> 4
    | XAttack -> 5
    | XDefense -> 6
    | XSpeed -> 7 in

  (*returns the number of copies of the item in the inventory*)
  let num_items_left item inv =
    (*returns the nth element of the given list*)
    let rec nth_entry lst n =
      match lst with 
      | [] -> 0 (*should not be reached for the purposes of PokeJouki*)
      | h::t -> if n = 1 then h else nth_entry lst (n - 1) in
    nth_entry inv (item_index item) in

  (*returns true if the given item can be used for the given steammon and team.
    false otherwise.*)
  let valid_item_use item mon td =
    let rec mon_present lst s =
      match lst with
      | [] -> false
      | h::t -> (h.species = s.species) || (mon_present t s) in
    let (sl, inv, cred) = td in
    if not (mon_present sl mon) then false else
    if num_items_left item inv < 1 then false else
    match item with
    | Revive -> mon.curr_hp = 0
    | _ -> mon.curr_hp > 0 in

  (*returns an inventory with one copy of the given item taken away from the
    given inventory. if the item is already at 0 copies, then it stays 
    at 0. *)
  let consume_item item inv =
    let rec replace_el e lst i n =
      match lst with 
      | [] -> []
      | h::t -> if i = n then e::t else h::(replace_el e t (i+1) n) in
    let amount = num_items_left item inv in
    if amount = 0 then inv else replace_el (amount - 1) inv 1 (item_index item) in
 
  (*fuck add GUI updates*)
  (*the method called to perform an item action for a team.*)
  let use_item (sl, inv, cred) item s =

    let rec replace_mon lst mon =
      match lst with
      | [] -> []
      | h::t -> if h.species = mon.species then mon::t else h::(replace_mon t mon) in

    let inv' = consume_item item inv in
    if valid_item_use item s (sl, inv, cred) then 
      let active = (List.hd sl).species = s.species in 
      let s' = item_on_mon item s active in      
      (replace_mon sl s', inv', cred)
    else
      (sl, inv', cred) in

  (*if the given steammon is asleep, frozen, or confused, a recovered steammon
    is returned at a certain probability.*)
  let status_recover mon =
    let chance = (Random.int 100) + 1 in
    let recovered_mon = {mon with status = None} in
    match mon.status with
    | Some Asleep -> if chance <= cWAKE_UP_CHANCE then recovered_mon else mon
    | Some Frozen -> if chance <= cDEFROST_CHANCE then recovered_mon else mon
    | Some Confused -> if chance <= cSNAP_OUT_OF_CONFUSION then recovered_mon else mon
    | Some _
    | None -> mon in

  (*if the steammon is poisoned or burned, then it is returned with damage
    inflicted.*)
  let status_end mon =
    match mon.status with
    | Some Burned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cBURN_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        {mon with curr_hp = new_hp}
    | Some Poisoned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cPOISON_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        {mon with curr_hp = new_hp}
    | Some _
    | None -> mon in

  (*returns true if a steammon in the party is already asleep*)
  let sleep_clause sl : bool =
    List.fold_left (fun b mon -> mon.status = Some Asleep) false sl in

  (*returns true if a steammon in the party is already frozen*)
  let freeze_clause sl : bool =
    List.fold_left (fun b mon -> mon.status = Some Frozen) false sl in

  let modifier mon stt : float = 
    let stage = match stt with
    | Atk -> mon.mods.attack_mod
    | Def -> mon.mods.defense_mod
    | SpA -> mon.mods.spl_attack_mod
    | SpD -> mon.mods.spl_defense_mod
    | Spe -> mon.mods.speed_mod in
    multiplier_of_modifier stage in

  (*add modifiers*)
  let first_action rs bs : color =
    let rspeed = rs.speed + rs.mods.speed_mod
    and bspeed = bs.speed + bs.mods.speed_mod in
    if rspeed > bspeed then Red 
    else if bspeed > rspeed then Blue 
    else if Random.int 2 = 0 then Red 
    else Blue in

  let move_occur mon move_name : move option =
    (*exception if move not found?*)
    match mon.status, get_move_from_steammon mon move_name with
    | _ , None -> None
    | Some Asleep, _ -> None
    | Some Frozen, _ -> None
    | Some Paralyzed, Some m -> 
        if (Random.int 100 + 1) <= cPARALYSIS_CHANCE then None else Some m
    | Some Confused, Some m -> 
        if (Random.int 100 + 1) <= cSELF_ATTACK_CHANCE then Some (Table.find movepool "SelfAttack") else Some m
    | Some _ , Some m
    | None , Some m -> if m.pp_remaining < 1 then None else Some m in

  let move_hit m : bool =
    if m.power = 0 && m.target = User then true
    else if (Random.int 100 + 1) < m.accuracy then true
    else false in

  let move_dmg m user_mon target_mon : int = 
    if m.power = 0 then 0 else
    let stab = 
      match user_mon.first_type, user_mon.second_type with 
      | None, None -> 1.0
      | Some t, None -> if t = m.element then cSTAB_BONUS else 1.0
      | None, Some u -> if u = m.element then cSTAB_BONUS else 1.0
      | Some t, Some u -> if t = m.element || u = m.element then cSTAB_BONUS else 1.0
    and typemult = 
      snd (calculate_type_matchup m.element (target_mon.first_type, target_mon.second_type))
    and burn =
      match user_mon.status with
      | Some Burned -> if is_special m.element then 1.0 else cBURN_WEAKNESS
      | Some _ -> 1.0
      | None -> 1.0 
    and rand = (Random.int (100 - cMIN_DAMAGE_RANGE + 1)) + cMIN_DAMAGE_RANGE in
    let multiplier = stab*.typemult*.burn*.(float_of_int rand)/.(100.0) in

    let atk = if is_special m.element then 
      int_of_float( (float_of_int user_mon.spl_attack)*.(modifier user_mon SpA) ) 
    else
      int_of_float( (float_of_int user_mon.attack)*.(modifier user_mon Atk) ) in

    let def = if is_special m.element then
      int_of_float( (float_of_int target_mon.spl_defense)*.(modifier target_mon SpD) )
    else
      int_of_float( (float_of_int target_mon.defense)*.(modifier target_mon Def) ) in

    (calculate_damage atk def m.power multiplier) in

  match g, ra, ba with 
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) ->
      initialize gsd rName bName
  | Game gsd, Action(SendTeamName (rName)), DoNothing -> 
      initialize gsd rName "Blue"
  | Game gsd, DoNothing, Action(SendTeamName (bName)) -> 
      initialize gsd "Red" bName
  | Game gsd, Action(PickSteammon name), DoNothing 
  | Game gsd, DoNothing, Action(PickSteammon name) -> draft gsd name 
  | Game gsd, Action(PickInventory (invlst1)), Action(PickInventory (invlst2))->
      inventory gsd invlst1 invlst2 false

  (*battle phase*)
    (*first step: both players select their starting pokemon*)
  (* | Game (rtd, btd), Action(SelectStarter (rs)), Action(SelectStarter (bs)) -> 
      let gsd' = (switch_in rtd rs, switch_in btd bs) in
      (None, gsd', Some(ActionRequest gsd'), Some(ActionRequest gsd')) 
    
  | Game gsd, UseMove rmove, UseMove bmove ->
  | Game gsd, UseMove rmove, UseItem (bi, bs) ->
  | Game gsd, UseItem (ri, rs), UseMove bmove ->
  | Game gsd, UseMove rmove, SwitchSteammon bs ->
  | Game gsd, SwitchSteammon rs, UseMove bmove ->

  (*note: status effects at the end of the turn are not taken care of*)
  | Game (rtd, btd), UseItem (ri, rs), UseItem (bi, bs) ->
      let gsd' = (use_item rtd ri rs, use_item btd, bi, bs) in
      (None, gsd', Some(ActionRequest gsd'), Some(ActionRequest gsd'))

  | Game (rtd, btd), SwitchSteammon rs, SwitchSteammon bs ->
      let gsd' = (switch_in rtd rs, switch_in btd bs) in
      (None, gsd', Some(ActionRequest gsd'), Some(ActionRequest gsd'))

  | Game (rtd, btd), UseItem (ri, rs), SwitchSteammon bs ->
      let gsd' = (use_item rtd ri rs, switch_in btd bs) in
      (None, gsd', Some(ActionRequest gsd'), Some(ActionRequest gsd'))

  | Game (rtd, btd), SwitchSteammon rs, UseItem (bi, bs) ->
      let gsd' = (switch_in rtd rs, use_item btd bi bs) in
      (None, gsd', Some(ActionRequest gsd'), Some(ActionRequest gsd')) *)


  | Game gsd, DoNothing, DoNothing -> exception_handle gsd 
  | _ -> failwith "swag"


  let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
        TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))