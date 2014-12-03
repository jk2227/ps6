open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to your own state type*)
type game = Game of game_status_data
type andy = {
  mutable ra: steammon;
  mutable ba: steammon;
  mutable rp: steammon list;
  mutable bp: steammon list;
  mutable rinv: int list;
  mutable binv: int list;
  credits: int*int
}
let init = ref 1
let drafting = ref 0
let invent = ref 0
let battling = ref 0 
let draftRD = ref 1
let draftColor = ref Red 
let draftpool = Initialization.mon_table 
let movepool = Initialization.move_table
let first = ref Red
type move_result_mut = {
  mutable name: string;
  mutable element: steamtype;
  mutable from: color;
  mutable toward: color;
  mutable damage: int;
  mutable hit: hit_result;
  mutable effectiveness: effectiveness;
  mutable effects: (effect_result * color) list;
  mutable dummy: int
}
let result = {name="wumpus"; element=Typeless; from=Red; toward=Blue;
              damage=0; hit=Hit; effectiveness=Regular; effects=[]; dummy=0}

let game_to_andy g : andy =
  let Game gsd = g in
  let (rtd, btd) = gsd in
  let (rsl, ri, rc ) = rtd and (bsl, bi, bc ) = btd in
  { 
    ra=(List.hd rsl);
    ba=(List.hd bsl);
    rp=(List.tl rsl);
    bp=(List.tl bsl);
    rinv=ri;
    binv=bi;
    credits = (rc, bc)
  }

let andy_to_gsd a : game_status_data =
  let rtd = (a.ra::a.rp, a.rinv, fst a.credits)
  and btd = (a.ba::a.bp, a.binv, snd a.credits) in
  (rtd, btd)

let move_result_convert (mrm: move_result_mut) : move_result =
  {name=mrm.name; element=mrm.element; from=mrm.from; toward=mrm.toward;
   damage=mrm.damage; hit=mrm.hit; effectiveness=mrm.effectiveness; 
   effects=mrm.effects}

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

  let rec get_mon lst name : steammon option =
    match lst with
    | [] -> None
    | h::t -> if h.species = name then Some h else get_mon t name in

  (*takes a team_data and a steammon. returns a team_data with the given
    steammon placed at the head of the steammon list (as the active pokemon) 
    the pokemon switched out has its modifiers removed. *)

  let switch_in s_name color data : unit =

    let switch_helper sl : steammon list =
      let no_mods = {attack_mod=0; defense_mod=0; spl_attack_mod=0; spl_defense_mod=0; speed_mod=0} in
      let out_mon = List.hd sl in
      let sl' = {out_mon with mods = no_mods}::(List.tl sl) in
      (*returns the given list with the given mon removed. preserves order.*)
      let rec remove_mon lst mon : steammon list = 
        match lst with
        | [] -> []
        | h::t -> if h.species = mon.species then t else h::(remove_mon t mon) in
      match get_mon sl' s_name with
      | None -> sl (*valid steammon not selected*)
      | Some in_mon -> in_mon::(remove_mon sl' in_mon) in

    match color with
    | Red -> let team = switch_helper (data.ra::data.rp) in
        data.ra <- List.hd team;
        data.rp <- List.tl team;
        add_update(SetChosenSteammon data.ra.species);
    | Blue -> let team = switch_helper (data.ba::data.bp) in
        data.ba <- List.hd team; 
        data.bp <- List.tl team;
        add_update(SetChosenSteammon data.ba.species); in

  (*uses the given item on the given steammon and returns the altered 
    steammon. 'active' is a boolean that is true if the given steammon
    is currently an active steammon and false otherwise. assumes that this item
    usage does not violate any rules. *)
  let item_on_mon item mon active color : steammon = 

    (*applies ether to a single move. returns the move with additional pp.*)
    let ether move = 
      let new_pp = min (move.pp_remaining + 5) move.max_pp in
      {move with pp_remaining = new_pp} in

    match item with
    | Ether -> add_update(Item ("Ether", RestoredPP 5, color, mon.species));
               { mon with first_move = ether mon.first_move;
                          second_move = ether mon.second_move;
                          third_move = ether mon.third_move;
                          fourth_move = ether mon.fourth_move
               }
    | MaxPotion -> add_update(Item ("MaxPotion", Recovered (mon.max_hp - mon.curr_hp), color, mon.species));
        {mon with curr_hp = mon.max_hp}
    | Revive -> add_update(Item ("Revive", Recovered (mon.max_hp/2), color, mon.species));
        {mon with curr_hp = mon.max_hp/2 ; status = None}
    | FullHeal -> begin match mon.status with 
        | None -> ()
        | Some sts -> add_update (Item ("FullHeal", HealedStatus sts, color, mon.species))
      end; {mon with status = None}

    | XAttack when active -> add_update(Item ("XAttack", StatModified (Atk, 1), color, mon.species));
      let mods' = {mon.mods with attack_mod = mon.mods.attack_mod + 1} in
      {mon with mods = mods'}
    | XDefense when active -> add_update(Item ("XDefense", StatModified (Def, 1), color, mon.species));
      let mods' = {mon.mods with defense_mod = mon.mods.defense_mod + 1} in
      {mon with mods = mods'}
    | XSpeed when active -> add_update(Item ("XSpeed", StatModified (Spe, 1), color, mon.species));
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
  let valid_item_use item s_name sl inv =
    
    (*let rec mon_present lst s_name =
      match lst with
      | [] -> false
      | h::t -> (h.species = s_name) || (mon_present t s) in*)
    if num_items_left item inv < 1 then false else
    match get_mon sl s_name , item with
    | None , _ -> false
    | Some mon , Revive -> mon.curr_hp = 0
    | Some mon, _ -> mon.curr_hp > 0 in

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
  let use_item item s_name color data : unit =

    let rec replace_mon lst mon : steammon list =
      match lst with
      | [] -> []
      | h::t -> if h.species = mon.species then mon::t else h::(replace_mon t mon) in

    match color with
    | Red when valid_item_use item s_name (data.ra::data.rp) data.rinv ->
        data.rinv <- consume_item item data.rinv;
        if s_name = data.ra.species then begin
          data.ra <- item_on_mon item data.ra true Red;
          add_update(UpdateSteammon (data.ra.species, data.ra.curr_hp, data.ra.max_hp, Red)) end
        else begin 
          match get_mon data.rp s_name with
          | None -> failwith "unreachable"
          | Some mon ->
              data.rp <- replace_mon data.rp (item_on_mon item mon false Red);
              add_update(UpdateSteammon (mon.species, mon.curr_hp, mon.max_hp, Red)) 
        end    
    | Blue when valid_item_use item s_name (data.ba::data.bp) data.binv ->
        data.binv <- consume_item item data.binv;
        if s_name = data.ba.species then begin
          data.ba <- item_on_mon item data.ba true Blue;
          add_update(UpdateSteammon (data.ba.species, data.ba.curr_hp, data.ba.max_hp, Blue)) end
        else begin 
          match get_mon data.bp s_name with
          | None -> failwith "unreachable"
          | Some mon -> 
              data.bp <- replace_mon data.bp (item_on_mon item mon false Blue);
              add_update(UpdateSteammon (mon.species, mon.curr_hp, mon.max_hp, Blue))
        end
    | _ -> () (*invalid item usage*) in

  (*if the given steammon is asleep, frozen, or confused, a recovered steammon
    is returned at a certain probability.*)
  let status_recover mon color : steammon =
    let chance = (Random.int 100) + 1 in
    let recovered_mon = {mon with status = None} in
    match mon.status with
    | Some Asleep -> if chance <= cWAKE_UP_CHANCE then begin
        add_update(AdditionalEffects [(HealedStatus Asleep, color)]);
        recovered_mon end
        else mon
    | Some Frozen -> if chance <= cDEFROST_CHANCE then begin
        add_update(AdditionalEffects [(HealedStatus Frozen, color)]);
        recovered_mon end
        else mon
    | Some Confused -> if chance <= cSNAP_OUT_OF_CONFUSION then begin
        add_update(AdditionalEffects [(HealedStatus Confused, color)]);
        recovered_mon end
        else mon
    | Some _
    | None -> mon in

  (*if the steammon is poisoned or burned, then it is returned with damage
    inflicted.*)
  let status_end mon color : steammon =
    match mon.status with
    | Some Burned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cBURN_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        add_update(AdditionalEffects [(DamagedByStatus(dmg, Burned), color)] );
        {mon with curr_hp = new_hp}
    | Some Poisoned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cPOISON_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        add_update(AdditionalEffects [(DamagedByStatus(dmg, Poisoned), color)] );
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
    let speed s = 
      if s.status = Some Paralyzed then 
        int_of_float( (float_of_int s.speed) *. (multiplier_of_modifier s.mods.speed_mod) /. 4. )
      else
        int_of_float( (float_of_int s.speed) *. (multiplier_of_modifier s.mods.speed_mod) ) in 
    let rspeed = speed rs
    and bspeed = speed bs in
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

  let move_effects (el, target, chance) (user, opp) dmg attacker : steammon*steammon =
    let color = if target = User then attacker else invert_color attacker in
    let stat_mod mods stt i : modifier =
      let alter_stage old_stage add =
        if i > 0 then min 6 (old_stage + add) else max (-6) (old_stage + add) in
      match stt with
      | Atk -> {mods with attack_mod = alter_stage mods.attack_mod i}
      | Def -> {mods with defense_mod = alter_stage mods.defense_mod i}
      | SpA -> {mods with spl_attack_mod = alter_stage mods.spl_attack_mod i}
      | SpD -> {mods with spl_defense_mod = alter_stage mods.spl_defense_mod i}
      | Spe -> {mods with speed_mod = alter_stage mods.speed_mod i} in

    let add_pp m i : move =
      {m with pp_remaining = min (m.pp_remaining + i) m.max_pp} in

    let single_effect (t: steammon) eff : steammon =
      match eff with
      | InflictStatus s -> 
          result.effects <- result.effects@[(InflictedStatus s, color)];
          {t with status = Some s}
      | StatModifier (s, i) -> {t with mods = stat_mod t.mods s i} (*assume that we just add i*)
      | RecoverPercent p -> 
          let new_hp = min (t.curr_hp + t.max_hp*p/100) t.max_hp in
          result.effects <- result.effects@[(Recovered (new_hp - t.curr_hp), color)];
          {t with curr_hp = new_hp } (*integer division*)
      | Recoil r ->
          let new_hp = max (t.curr_hp - dmg*r/100) 0 in
          result.effects <- result.effects@[(Recoiled (t.curr_hp - new_hp), color)];
          {t with curr_hp = new_hp}
      | DamagePercent p -> 
          let new_hp = max (t.curr_hp - t.max_hp*p/100) 0 in
          result.effects <- result.effects@[(Damaged (t.curr_hp - new_hp), color)];
          {t with curr_hp = new_hp}
      | HealStatus sl -> begin 
          match t.status with 
          | None -> t 
          | Some s -> 
            if List.mem s sl then begin 
              result.effects <- result.effects@[(HealedStatus s, color)];
              {t with status = None} end
            else t 
        end
      | RestorePP i -> 
          result.effects <- result.effects@[(RestoredPP i, color)];
          { t with first_move = add_pp t.first_move i; 
                   second_move = add_pp t.second_move i;
                   third_move = add_pp t.third_move i;
                   fourth_move = add_pp t.fourth_move i } in

    if ((Random.int 100) + 1) <= chance then 
      begin match target with
        | User -> (List.fold_left (fun mon eff -> single_effect mon eff) user el, opp)
        | Opponent -> (user, List.fold_left (fun mon eff -> single_effect mon eff) opp el)
      end
    else (user, opp) in

  let perform_move move_name (rs, bs) attacker data : unit =
    let dmg_and_effects (user, opp) move : steammon*steammon =
      match move.target with
      | User -> let dmg = move_dmg move user user in
        let matchup = calculate_type_matchup move.element (user.first_type, user.second_type) in
        result.damage <- dmg;
        result.effectiveness <- (fst matchup);
        if dmg >= user.curr_hp then ({user with curr_hp = 0}, opp) else begin
          let user' = {user with curr_hp = user.curr_hp - dmg} in
          match move.effects with
          | None -> (user', opp)
          | Some eff -> move_effects eff (user', opp) dmg attacker
        end   
      | Opponent -> let dmg = move_dmg move user opp in
        let matchup = calculate_type_matchup move.element (opp.first_type, opp.second_type) in
        result.damage <- dmg;
        result.effectiveness <- (fst matchup);
        if dmg >= opp.curr_hp then (user, {opp with curr_hp = 0}) else begin
          let opp' = {opp with curr_hp = opp.curr_hp - dmg} in
          match move.effects with
          | None -> (user, opp')
          | Some eff -> move_effects eff (user, opp') dmg attacker
        end in

    let move_helper (user, opp) : steammon*steammon =
      match move_occur user move_name with
      | None -> (user, opp)
      | Some move when (move_hit move) -> 
          result.name <- move.name;
          result.element <- move.element;
          result.from <- attacker;
          result.toward <- (if move.target = User then attacker else invert_color attacker);
          result.hit <- Hit;
          result.effects <- [];
          dmg_and_effects (user, opp) move
      | Some move -> 
          result.name <- move.name;
          result.element <- move.element;
          result.from <- attacker;
          result.toward <- (if move.target = User then attacker else invert_color attacker);
          result.damage <- 0;
          result.hit <- Miss;
          result.effectiveness <- Regular;
          result.effects <- [];
          (user, opp) in

    match attacker with
    | Red -> let (rs', bs') = move_helper (rs, bs) in
        data.ra <- rs';
        data.ba <- bs';
        add_update(Move (move_result_convert result));
        add_update(UpdateSteammon (rs'.species, rs'.curr_hp, rs'.max_hp, Red));
        add_update(UpdateSteammon (bs'.species, bs'.curr_hp, bs'.max_hp, Blue));

    | Blue -> let (bs', rs') = move_helper (bs, rs) in
        data.ra <- rs';
        data.ba <- bs';
        add_update(Move (move_result_convert result)); 
        add_update(UpdateSteammon (rs'.species, rs'.curr_hp, rs'.max_hp, Red));
        add_update(UpdateSteammon (bs'.species, bs'.curr_hp, bs'.max_hp, Blue)); in

  let check_faint (user, opp) : bool =
    user.curr_hp <= 0 || opp.curr_hp <= 0 in

  (*called if either steammon has fainted. does end-of-turn status effects on
    any alive steammon and returns the appropriate game_output *)
  let rec faint_response data (rs, bs) : game_output =
    match rs.curr_hp , bs.curr_hp with 
    | 0 , 0 -> let gsd = andy_to_gsd data in 
      (None, gsd, Some (Request (StarterRequest gsd)), Some ( Request(StarterRequest gsd)))

    | 0 , _ -> let bs' = status_end bs Blue in data.ba <- bs';
      if bs'.curr_hp <= 0 then faint_response data (rs, bs') else
      let gsd = andy_to_gsd data in
      (None, gsd, Some (Request(StarterRequest gsd)), None)

    | _ , 0 -> let rs' = status_end rs Red in data.ra <- rs';
      if rs'.curr_hp <= 0 then faint_response data (rs', bs) else
      let gsd = andy_to_gsd data in
      (None, gsd, None, Some (Request(StarterRequest gsd)))

    | _ , _ -> failwith "should not be reached" in

  (*assumes both steammon have not fainted yet. does end-of-turn status effects
    on both steammon and returns the appropriate game_output*)
  let alive_response data (rs, bs) : game_output =
    let rs' = status_end rs Red and bs' = status_end bs Blue in
    data.ra <- rs';
    data.ba <- bs';
    let gsd = andy_to_gsd data in
    match rs'.curr_hp , bs'.curr_hp with
    | 0 , 0 -> (None, gsd, Some (Request(StarterRequest gsd)), Some (Request(StarterRequest gsd)))
    | 0 , _ -> (None, gsd, Some (Request(StarterRequest gsd)), None)
    | _ , 0 -> (None, gsd, None, Some (Request(StarterRequest gsd)))
    | _ , _ -> (None, gsd, Some (Request(ActionRequest gsd)), Some (Request(ActionRequest gsd))) in

  let data = game_to_andy g in
  if !battling = 1 then 
    first := first_action data.ra data.ba;
    add_update (SetFirstAttacker !first);

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
  | Game (rtd, btd), Action(SelectStarter (rs)), Action(SelectStarter (bs)) -> 
      switch_in rs Red data; switch_in bs Blue data;
      alive_response data (data.ra, data.ba)
    
  | Game gsd, Action (UseMove rmove), Action (UseMove bmove) -> begin
      match !first with
      | Red -> perform_move rmove (data.ra, data.ba) Red data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else begin
            perform_move bmove (data.ra, data.ba) Blue data;
            if check_faint (data.ra, data.ba) then
              faint_response data (data.ra, data.ba) 
            else
              alive_response data (data.ra, data.ba) end

      | Blue -> perform_move bmove (data.ra, data.ba) Blue data;
          if check_faint (data.ra, data.ba) then
            faint_response data (data.ra, data.ba)
          else begin
            perform_move rmove (data.ra, data.ba) Red data;
            if check_faint (data.ra, data.ba) then
              faint_response data (data.ra, data.ba)
            else
              alive_response data (data.ra, data.ba) end
      end

  | Game gsd, Action (UseMove rmove), Action (UseItem (bi, bs_name)) -> begin
      match !first with
      | Red -> perform_move rmove (data.ra, data.ba) Red data;
          if data.ra.curr_hp <= 0 then begin
            use_item bi bs_name Blue data;
            faint_response data (data.ra, data.ba) end
          else if data.ba.curr_hp <= 0 then faint_response data (data.ra, data.ba)
               else begin use_item bi bs_name Blue data;
                    alive_response data (data.ra, data.ba) end
      | Blue -> use_item bi bs_name Blue data;
          perform_move rmove (data.ra, data.ba) Red data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else alive_response data (data.ra, data.ba)
      end

  | Game gsd, Action( UseItem (ri, rs_name)), Action (UseMove bmove) -> begin
      match !first with
      | Red -> use_item ri rs_name Red data;
          perform_move bmove (data.ra, data.ba) Blue data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else alive_response data (data.ra, data.ba)
      | Blue -> perform_move bmove (data.ra, data.ba) Blue data;
          if data.ba.curr_hp <= 0 then begin
            use_item ri rs_name Red data;
            faint_response data (data.ra, data.ba) end
          else if data.ra.curr_hp <= 0 then faint_response data (data.ra, data.ba)
               else begin use_item ri rs_name Red data;
                    alive_response data (data.ra, data.ba) end
      end

  (*note: fix case where one mon faints and the other mon uses a self-move*)

  | Game gsd, Action (UseMove rmove), Action (SwitchSteammon bs_name) -> begin
      match !first with
      | Red -> perform_move rmove (data.ra, data.ba) Red data;
          if data.ra.curr_hp <= 0 then begin
            switch_in bs_name Blue data;
            faint_response data (data.ra, data.ba) end
          else if data.ba.curr_hp <= 0 then faint_response data (data.ra, data.ba)
               else begin switch_in bs_name Blue data;
                    alive_response data (data.ra, data.ba) end
      | Blue -> switch_in bs_name Blue data;
          perform_move rmove (data.ra, data.ba) Red data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else alive_response data (data.ra, data.ba)
      end

  | Game gsd, Action (SwitchSteammon rs_name), Action (UseMove bmove) -> begin
      match !first with
      | Red -> switch_in rs_name Red data;
          perform_move bmove (data.ra, data.ba) Blue data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else alive_response data (data.ra, data.ba)
      | Blue -> perform_move bmove (data.ra, data.ba) Blue data;
          if data.ba.curr_hp <= 0 then begin
            switch_in rs_name Red data;
            faint_response data (data.ra, data.ba) end
          else if data.ra.curr_hp <= 0 then faint_response data (data.ra, data.ba)
               else begin switch_in rs_name Red data; 
                    alive_response data (data.ra, data.ba) end
      end

  | Game gsd, Action (UseItem (ri, rs)), Action (UseItem (bi, bs)) -> begin
      match !first with
      | Red -> use_item ri rs Red data; use_item bi bs Blue data
      | Blue -> use_item bi bs Blue data; use_item ri rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action (SwitchSteammon rs), Action (SwitchSteammon bs) -> begin
      match !first with
      | Red -> switch_in rs Red data; switch_in bs Blue data
      | Blue -> switch_in bs Blue data; switch_in rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action (UseItem (ri, rs)), Action (SwitchSteammon bs) -> begin
      match !first with
      | Red -> use_item ri rs Red data; switch_in bs Blue data
      | Blue -> switch_in bs Blue data; use_item ri rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action (SwitchSteammon rs), Action (UseItem (bi, bs)) -> begin
      match !first with
      | Red -> switch_in rs Red data; use_item bi bs Blue data
      | Blue -> use_item bi bs Blue data; switch_in rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, DoNothing, DoNothing -> exception_handle gsd 
  | _ -> failwith "swag"


let init_game () =
  init_pool ("moves.csv") ("steammon.csv");
  (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
      TeamNameRequest,TeamNameRequest, 
      hash_to_list (Initialization.move_table), 
      hash_to_list(draftpool))