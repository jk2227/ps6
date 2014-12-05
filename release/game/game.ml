open Definitions
open Util
open Constants
open Netgraphics
open Initialization
open Studentutil

(* You have to implement this. Change it from int to your own state type*)
type game = Game of game_status_data
type game_state = Init | Drafting | Inventory | Battle | BattleSelect | Winner

type battle_data = {
  mutable ra: steammon;
  mutable ba: steammon;
  mutable rp: steammon list;
  mutable bp: steammon list;
  mutable rinv: int list;
  mutable binv: int list;
  mutable credits: int*int
}
type game_state = Init | Drafting | Inventory | Battle | BattleSelect | Winner
let battle_error = ref false
let state = ref Init
let draftRD = ref 1
let draftColor = ref Blue 
let draftpool = Initialization.mon_table 
let movepool = Initialization.move_table
let first = ref Red
let data = {
  ra=Studentutil.dummymon;
  ba=Studentutil.dummymon;
  rp=[];
  bp=[];
  rinv=[];
  binv=[];
  credits=(31,10)
}

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

let battle_error = ref false
let state = ref Init
let draftRD = ref 1
let draftColor = ref Red 
let draftpool = Initialization.mon_table 
let movepool = Initialization.move_table
let first = ref Red

(*
 * Convert to the game_status_data defined in definitions.ml
 *)
let game_datafication g = match g with 
  | Game x -> x 

(*
 * Convert to type game from game_status_data
 *)  
let game_from_data game_data = Game (game_data)

(*takes a value of type game and copies the information into the 
  mutable fields of data, which is of type battle_data *)
let copy_game_to_data g data : unit =
  let Game gsd = g in
  let (rtd, btd) = gsd in
  let (rsl, ri, rc ) = rtd and (bsl, bi, bc ) = btd in
  data.ra <- (List.hd rsl);
  data.ba <- (List.hd bsl);
  data.rp <- (List.tl rsl);
  data.bp <- (List.tl bsl);
  data.rinv <- ri;
  data.binv <- bi;
  data.credits <- (rc, bc)

(*takes a value of type battle_data, constructs a corresponding
  value of type game_status_data, and returns it.*)
let data_to_gsd d : game_status_data =
  let rtd = (d.ra::d.rp, d.rinv, fst d.credits)
  and btd = (d.ba::d.bp, d.binv, snd d.credits) in
  (rtd, btd)

(*takes a value of the record type move_result_mut and converts
  it into a value of type move_result *)
let move_result_convert (mrm: move_result_mut) : move_result =
  {name=mrm.name; element=mrm.element; from=mrm.from; toward=mrm.toward;
   damage=mrm.damage; hit=mrm.hit; effectiveness=mrm.effectiveness; 
   effects=mrm.effects}

(* handle_step gs ra ba is used to generate the next state of the game.
 * gs is the current game state entering this turn.
 * ra and ba are the Action commands sent by the red team and the blue
 *   team, respectively. If ra and ba are not Action(action) commands,
 *   the game will ignore the command given.
 * handle_step outputs game_output (gr, gs, rr, br) where:
 * gr is the result of the game. Output None if no winner was determined.
 * gs is the state of the game after handle_step ended.
 * rr is an option containing the request for the red team
 * br is an option containing the request for the blue team
 * None indicates that the team should respond with DoNothing
 *)
let handle_step g ra ba = 
  
  (*for drafting purposes, sets the player to draft next if the round 
  that we are on is odd*)
  let changeColor () = 
    if !draftRD mod 2 = 1 then begin
      draftColor := invert_color !draftColor; 
      (1=1)
    end 
    else 
      (0=1) 
  in 

  (*adds draft related update to the gui, remove the steammon from the
  draftpool and increments the draft round accordingly*)
  (*requires: a steammon to draft and the color of the person drafting*)
  (*returns: a unit*)
  let draftupdate s c=
    add_update(UpdateSteammon (s.species, s.curr_hp, s.max_hp, c)); 
    Table.remove draftpool s.species; 
    draftRD := !draftRD + 1; ()
  in
  
  (*finds steammon of minimum cost in the available draft pool*)
  (*requires: unit*)
  (*returns: the steammon with minimum cost*)
  let findMin () = 
    let lst = hash_to_list(draftpool) in
      let sorted = List.sort (fun a b -> a.cost - b.cost) lst in 
        List.hd sorted   
   in
  
  (*returns the appropriate game status data given the current game_status_data
  *)
  (*requires: a game status data, the color of team drafting, a steammon to 
  draft, and a bool indicating whether credits should be set to 0 or not*)
  let draftGSD ((rsl,ri,rcred),(bsl,bi,bcred)) c s z = 
    match c, z with 
    | Red, true -> (((s::rsl),ri,0), (bsl,bi,bcred))
    | Red, false -> (((s::rsl),ri,(rcred-s.cost)), (bsl,bi,bcred))   
    | Blue, true -> ((rsl,ri,rcred), (s::bsl,bi,0))
    | Blue, false -> ((rsl,ri,rcred),(s::bsl,bi,bcred-s.cost))
  in
  
  (*apprpriately ends the draft phase and transitions to the inventory phase*)
  (*requires: game status data of the draft phase*)
  (*returns: a game status data going to inventory phase*)
  let endDraft gsd =
    state := Inventory;
    (None, gsd, Some(Request(PickInventoryRequest gsd)), 
      Some(Request(PickInventoryRequest gsd)))
  in

  (*main helper method for draft to avoid repetetive code*)
  (*requires: game status data, the color of team drafting, steammon that is 
  being drafted, a bool that checks the end of the game, and a bool that
  checks whether cost hits 0 or not*)
  (*returns: an apprpriate game_output*)
  let draftHelper gsd ogColor s checkEnd zero = 
     let switch = changeColor () in 
     let pr =  Some(Request(PickRequest(!draftColor,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))) in 
     draftupdate s ogColor; 
     let gsd = draftGSD gsd ogColor s zero in 
     if checkEnd then 
        endDraft gsd 
     else match ogColor, switch with 
     | Red, true 
     | Blue, false -> (None, gsd, None, pr)
     | Red, false 
     | Blue, true -> (None, gsd, pr , None)
  in
  (*handles the draft part*)
  (*requires: game status data and name of steammon to draft*)
  (*returns: an appropriate game status data representing the current state of
  the game*)
  let draft ((rsl,ri,rcred),(bsl,bi,bcred)) name = 
      let checkEnd = ((List.length rsl = cNUM_PICKS-1) && 
      (List.length bsl = cNUM_PICKS)) || ((List.length rsl = cNUM_PICKS) && 
      (List.length bsl = cNUM_PICKS-1)) in 
      let s = if Table.mem draftpool name = false then findMin () 
        else Table.find draftpool name 
      in 
      match !draftColor with 
      | Red when List.length rsl < cNUM_PICKS -> 
        if s.cost < rcred then 
          draftHelper ((rsl,ri,rcred),(bsl,bi,bcred)) Red s checkEnd false
        else let mon = findMin () in 
          draftHelper ((rsl,ri,rcred),(bsl,bi,bcred)) Red mon checkEnd 
            (mon.cost > rcred)

      | Blue when List.length bsl < cNUM_PICKS -> 
        if s.cost < bcred then 
          draftHelper ((rsl,ri,rcred),(bsl,bi,bcred)) Blue s checkEnd false
        else let mon = findMin () in 
          draftHelper ((rsl,ri,rcred),(bsl,bi,bcred)) Blue mon checkEnd 
            (mon.cost > bcred)
       | _ -> failwith "should never reach here"
      in

  (*takes in a inventory list and if there is no exception then just returns
  that list if cost permits; otherwise return the default list defined in 
  definitions.ml*)
  (*requires: a list of inventory and a bool that specifis whether there is an
  exception or not*)
  (*returns: a list of inventory that is proper for the game state*)
  let validInventory invlst exc = 
    let default = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE;
       cNUM_FULL_HEAL; cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in 
    if exc then 
      default 
    else 
      let itemlst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_REVIVE;
      cCOST_FULLHEAL; cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in 
      let cost = List.fold_left2(fun acc a b -> acc+(a*b)) 0 invlst itemlst in 
      if cost > cINITIAL_CASH then 
        default 
      else invlst 
  in

  (*handles inventory phase, sends proper gui updates, and transitions to the 
  battle phase*)
  (*requires: game status data, the inventory list of desire of red and b, 
  and two bool values that specify whether there exception should be handled*)
  (*returns: a game status data that starts the battle phase*)
  let inventory ((rsl,ri,rcred),(bsl,bi,bcred)) invlstR invlstB excR excB = 
      state := BattleSelect;
      copy_game_to_data g data;
      let r_inv = validInventory invlstR excR in 
      let b_inv = validInventory invlstR excB in 
      let gsd = ((rsl, r_inv, rcred), (bsl, b_inv, bcred)) in 
    (None, gsd, Some(Request(StarterRequest(gsd))), 
      Some(Request(StarterRequest(gsd))))
   in 

  (*initializes the game, sends proper gui updates, and transitions to the 
  draft phase*)
  (*requires: game status data, string representing name of red team and
  string representing name of blue team*)
  (*returns: game status data *)
  let initialize gsd red blue = 
    send_update (InitGraphics (red,blue));
    state := Drafting;
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

  in

  (*handles exception cases where the both players send doNothing*)
  (*require: game status data*)
  (*returns: a proper game status data*)
  let exception_handle gsd = 
    match !state with 
    | Init -> initialize gsd "Red" "Blue" 
    | Drafting -> let s = findMin () in draft gsd s.species
    | Inventory -> inventory gsd [] [] true true 
    | Battle -> begin 
      battle_error := true; 
      (None, gsd, Some (Request(ActionRequest gsd)), 
        Some (Request(ActionRequest gsd)))
    end
    | BattleSelect -> begin 
      if data.ra.curr_hp = 0 && data.ba.curr_hp = 0 then
        (None, gsd, Some (Request(StarterRequest gsd)), 
          Some (Request(StarterRequest gsd)))
      else if data.ra.curr_hp = 0 then
        (None, gsd, Some (Request(StarterRequest gsd)), None)
      else
        (None, gsd, None, Some (Request(StarterRequest gsd))) end
    | _ -> failwith "Something went really wrong..."
  in 

  (*makes the given steammon the active steammon for team 'color'.
    the steammon switched out has its modifiers removed. 
    if the designated steammon is not in the team, then nothing
    happens. *)
  let switch_in s_name color data : unit =
    (*helper function that returns the rearranged steammon list and the
      mods removed from the switched-out steammon *)
    let switch_helper sl : steammon list =
      let out_mon = List.hd sl in
      let sl' = {out_mon with mods = no_mods}::(List.tl sl) in
      (*returns the given list with the given mon removed. preserves order.*)
      let rec remove_mon lst mon : steammon list = 
        match lst with
        | [] -> []
        | h::t -> if h.species = mon.species then t 
                    else h::(remove_mon t mon) in
      (*gives the nth steammon in the steammon list*)
      let rec nth_mon lst n : steammon =
        match lst with
        | [] -> failwith "should not be reached"
        | h::t -> if n = 0 then h else nth_mon t (n - 1) in
      match get_mon sl' s_name with
      | None -> sl (*valid steammon not selected*)
      | Some in_mon -> 
          if in_mon.curr_hp > 0 then in_mon::(remove_mon sl' in_mon)
          else let sl'' = List.filter (fun mon -> mon.curr_hp > 0) sl' in
               let rand_mon = nth_mon sl'' (Random.int (List.length sl'')) in
               rand_mon::(remove_mon sl' rand_mon) in

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
    | MaxPotion -> add_update(Item ("MaxPotion", 
      Recovered (mon.max_hp - mon.curr_hp), color, mon.species));
        {mon with curr_hp = mon.max_hp}
    | Revive -> add_update(Item ("Revive", Recovered (mon.max_hp/2), 
      color, mon.species));
        {mon with curr_hp = mon.max_hp/2 ; status = None}
    | FullHeal -> begin match mon.status with 
        | None -> ()
        | Some sts -> add_update (Item ("FullHeal", 
          HealedStatus sts, color, mon.species))
      end; {mon with status = None}

    | XAttack when active -> add_update(Item ("XAttack", 
        StatModified (Atk, 1), color, mon.species));
      let mods' = {mon.mods with attack_mod = mon.mods.attack_mod + 1} in
      {mon with mods = mods'}
    | XDefense when active -> add_update(Item ("XDefense", 
        StatModified (Def, 1), color, mon.species));
      let mods' = {mon.mods with defense_mod = mon.mods.defense_mod + 1} in
      {mon with mods = mods'}
    | XSpeed when active -> add_update(Item ("XSpeed", 
        StatModified (Spe, 1), color, mon.species));
      let mods' = {mon.mods with speed_mod = mon.mods.speed_mod + 1} in
      {mon with mods = mods'}
    | _ -> mon in
 
  (*the method called to perform an item action for a team.
    side effects: an item may be consumed and a steammon's condition
    may change. *)
  let use_item item s_name color data : unit =
    (*if the list contains a steammon of the same species as the given
      steammon, then it is replaced with the given steammon and the new list
      is returned. if the steammon is not found, then the original list is
      returned. *)
    let rec replace_mon lst mon : steammon list =
      match lst with
      | [] -> []
      | h::t -> if h.species = mon.species then mon::t 
                else h::(replace_mon t mon) in

    match color with
    | Red when valid_item_use item s_name (data.ra::data.rp) data.rinv ->
        data.rinv <- consume_item item data.rinv;
        if s_name = data.ra.species then begin
          data.ra <- item_on_mon item data.ra true Red;
          add_update(UpdateSteammon (data.ra.species, data.ra.curr_hp, 
            data.ra.max_hp, Red)) end
        else begin 
          match get_mon data.rp s_name with
          | None -> failwith "unreachable"
          | Some mon ->
              data.rp <- replace_mon data.rp (item_on_mon item mon false Red);
              add_update(UpdateSteammon (mon.species, mon.curr_hp, 
                mon.max_hp, Red)) 
        end    
    | Blue when valid_item_use item s_name (data.ba::data.bp) data.binv ->
        data.binv <- consume_item item data.binv;
        if s_name = data.ba.species then begin
          data.ba <- item_on_mon item data.ba true Blue;
          add_update(UpdateSteammon (data.ba.species, data.ba.curr_hp, 
            data.ba.max_hp, Blue)) end
        else begin 
          match get_mon data.bp s_name with
          | None -> failwith "unreachable"
          | Some mon -> 
              data.bp <- replace_mon data.bp (item_on_mon item mon false Blue);
              add_update(UpdateSteammon (mon.species, mon.curr_hp, 
                mon.max_hp, Blue))
        end
    | _ -> () (*invalid item usage*) in

  (*returns true if a steammon in the party is already asleep*)
  let sleep_clause color data : bool =
    let sl = match color with
             | Red -> data.ra::data.rp
             | Blue -> data.ba::data.bp in
    List.fold_left (fun b mon -> mon.status = Some Asleep) false sl in

  (*returns true if a steammon in the party is already frozen*)
  let freeze_clause color data : bool =
    let sl = match color with
             | Red -> data.ra::data.rp
             | Blue -> data.ba::data.bp in
    List.fold_left (fun b mon -> mon.status = Some Frozen) false sl in

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
    if !state != Battle then mon else
    match mon.status with
    | Some Burned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cBURN_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        add_update(AdditionalEffects [(DamagedByStatus(dmg, Burned), color)] );
        {mon with curr_hp = new_hp}
    | Some Poisoned -> 
        let dmg = int_of_float( (float_of_int mon.max_hp) *. cPOISON_DAMAGE ) in
        let new_hp = if dmg > mon.curr_hp then 0 else mon.curr_hp - dmg in
        add_update(AdditionalEffects [(DamagedByStatus(dmg, Poisoned), color)]);
        {mon with curr_hp = new_hp}
    | Some _
    | None -> mon in

  (*determines if the given move occurs. if the steammon does not know
    the move, then None is returned. if the move is out of pp, then the move
    struggle is returned. note: this is not the step that determines whether
    the move hits or misses.*)
  let move_occur mon move_name : move option =
    (*exception if move not found?*)
    let struggle = Table.find movepool "Struggle" in
    match mon.status, get_move_from_steammon mon move_name with
    | _ , None -> None
    | Some Asleep, _ -> result.hit <- Failed Asleep; None
    | Some Frozen, _ -> result.hit <- Failed Frozen; None
    | Some Paralyzed, Some m -> 
        if (Random.int 100 + 1) <= cPARALYSIS_CHANCE then begin
          result.hit <- Failed Paralyzed; None end
        else if m.pp_remaining < 1 then Some struggle 
             else Some m
    | Some Confused, Some m -> 
        if (Random.int 100 + 1) <= cSELF_ATTACK_CHANCE then 
          Some (Table.find movepool "SelfAttack")
        else if m.pp_remaining < 1 then
          Some struggle
        else
          Some m
    | Some _ , Some m
    | None , Some m -> 
      if m.pp_remaining < 1 then begin 
       Some struggle 
      end 
      else Some m in

  (*performs move effects with the given active steammon pair and returns
    the modified steammon *)
  let move_effects (el, target, chance) (user, opp) dmg attacker data : steammon*steammon =
    let color = if target = User then attacker else invert_color attacker in
    (*takes a modifier and returns a modifier with i added to the given stat*)
    let stat_mod mods stt i : modifier =
      (*adds a value to the old_stage to produce the new stage, but also makes
        sure it stays between -6 and 6 *)
      let alter_stage old_stage add =
        if i > 0 then min 6 (old_stage + add) else max (-6) (old_stage + add) in
      match stt with
      | Atk -> let a = alter_stage mods.attack_mod i in
          result.effects <- 
          result.effects@[(StatModified(Atk, a - mods.attack_mod), color)];
          {mods with attack_mod = a}
      | Def -> let d = alter_stage mods.defense_mod i in
          result.effects <- 
          result.effects@[(StatModified(Def, d - mods.defense_mod), color)];
          {mods with defense_mod = d}
      | SpA -> let sa = alter_stage mods.spl_attack_mod i in 
          result.effects <- 
          result.effects@[(StatModified(SpA, sa - mods.spl_attack_mod), color)];
          {mods with spl_attack_mod = sa}
      | SpD -> let sd = alter_stage mods.spl_defense_mod i in
          result.effects <- 
          result.effects@[(StatModified(SpD, sd - mods.spl_defense_mod), color)];
          {mods with spl_defense_mod = sd}
      | Spe -> let sp = alter_stage mods.speed_mod i in
          result.effects <- 
          result.effects@[(StatModified(Spe, sp - mods.speed_mod), color)];
          {mods with speed_mod = sp} in

    (*increments the remaining pp of the given move and returns the altered
      move. pp_remaining cannot go over max_pp. *)
    let add_pp m i : move =
      {m with pp_remaining = min (m.pp_remaining + i) m.max_pp} in

    (*performs a single effect on a steammon and returns the altered steammon.*)
    let single_effect (t: steammon) eff : steammon =
      match eff with
      | InflictStatus s -> 
          if t.status != None then t else 
          if (s = Asleep && sleep_clause color data) || 
            (s = Frozen && freeze_clause color data) then
            t
          else begin
            result.effects <- result.effects@[(InflictedStatus s, color)];
            {t with status = Some s}
          end
      (*assume that we just add i*)
      | StatModifier (s, i) -> {t with mods = stat_mod t.mods s i} 
      | RecoverPercent p -> 
          let new_hp = min (t.curr_hp + t.max_hp*p/100) t.max_hp in
          result.effects <- 
            result.effects@[(Recovered (new_hp - t.curr_hp), color)];
          {t with curr_hp = new_hp } (*integer division*)
      | Recoil r ->
          let new_hp = max (t.curr_hp - dmg*r/100) 0 in
          result.effects <- 
            result.effects@[(Recoiled (t.curr_hp - new_hp), color)];
          {t with curr_hp = new_hp}
      | DamagePercent p -> 
          let new_hp = max (t.curr_hp - t.max_hp*p/100) 0 in
          result.effects <- 
            result.effects@[(Damaged (t.curr_hp - new_hp), color)];
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
        | User -> (List.fold_left 
            (fun mon eff -> single_effect mon eff) user el, opp)
        | Opponent -> (user, List.fold_left 
            (fun mon eff -> single_effect mon eff) opp el)
      end
    else (user, opp) in

  (*decrements the pp of the given move by one.
    side effects: the active steammon in the given team is updated
    with the decremented pp. *)
  let reduce_pp move color data : unit =
    let move' = {move with pp_remaining = (move.pp_remaining - 1)} in
    match color with
    | Red -> data.ra <- replace_move data.ra move'
    | Blue -> data.ba <- replace_move data.ba move' in

  (*performs a move with the given active steammon.
    side effects: updates the steammon information contained
    in data. *)
  let perform_move move_name (rs, bs) attacker data : unit =
    (*performs damage calculation and any move effects and returns the
      altered steammon
      side effects: records information about the move into 'result' *)
    let dmg_and_effects (user, opp) move : steammon*steammon =
      match move.target with
      | User -> let dmg = min (move_dmg move user user) user.curr_hp in
        let matchup = calculate_type_matchup move.element (user.first_type, 
                        user.second_type) in
        result.damage <- dmg;
        result.effectiveness <- (fst matchup);
        if dmg = 0 then result.effectiveness <- Regular;
        if dmg = user.curr_hp then ({user with curr_hp = 0}, opp) else begin
          let user' = {user with curr_hp = user.curr_hp - dmg} in
          match move.effects with
          | None -> (user', opp)
          | Some eff -> move_effects eff (user', opp) dmg attacker data
        end   
      | Opponent -> let dmg = min (move_dmg move user opp) opp.curr_hp in
        let matchup = calculate_type_matchup move.element (opp.first_type, 
                        opp.second_type) in
        result.damage <- dmg;
        result.effectiveness <- (fst matchup);
        if dmg = 0 && fst matchup != Ineffective then 
          result.effectiveness <- Regular;
        let opp' = {opp with curr_hp = opp.curr_hp - dmg} in
        match move.effects with
        | None -> (user, opp')
        | Some (el, etar, chance) ->
            if etar = User then 
              move_effects (el, etar, chance) (user, opp') dmg attacker data
            else if fst matchup = Ineffective || opp'.curr_hp = 0 then 
              (user, opp')
            else
              move_effects (el, etar, chance) (user, opp') dmg attacker data
        in

    (*helper method that returns the final altered steammon that result
      from the move execution*)
    let move_helper (user, opp) : steammon*steammon =
      match move_occur user move_name with
      | None -> 
          result.name <- move_name;
          result.element <- Typeless;
          result.from <- attacker;
          result.toward <- invert_color attacker;
          result.damage <- 0;
          result.effectiveness <- Regular;
          result.effects <- [];
          (user, opp)
      | Some move when (move_hit move) -> 
          reduce_pp move attacker data;
          result.name <- move.name;
          result.element <- move.element;
          result.from <- attacker;
          result.toward <- (if move.target = User then 
                               attacker 
                            else 
                               invert_color attacker);
          result.hit <- Hit;
          result.effects <- [];
          let user' = if attacker = Red then 
                        data.ra 
                      else 
                        data.ba in
          dmg_and_effects (user', opp) move
      | Some move -> 
          reduce_pp move attacker data;
          result.name <- move.name;
          result.element <- move.element;
          result.from <- attacker;
          result.toward <- (if move.target = User then 
                              attacker 
                            else 
                              invert_color attacker);
          result.damage <- 0;
          result.hit <- Miss;
          result.effectiveness <- Regular;
          result.effects <- [];
          let user' = if attacker = Red then 
                        data.ra 
                      else 
                        data.ba in
          (user', opp) in

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
        add_update(UpdateSteammon (bs'.species, bs'.curr_hp, bs'.max_hp, Blue)); 
      in

  (*called if either steammon has fainted. does end-of-turn status effects on
    any alive steammon and returns the appropriate game_output *)
  let rec faint_response data (rs, bs) : game_output =
    battle_error := false;
    state := BattleSelect;
    add_update(UpdateSteammon (rs.species, rs.curr_hp, rs.max_hp, Red));
    add_update(UpdateSteammon (bs.species, bs.curr_hp, bs.max_hp, Blue));
    match rs.curr_hp , bs.curr_hp with 
    | 0 , 0 -> let gsd = data_to_gsd data in 
      (None, gsd, Some (Request (StarterRequest gsd)), 
        Some (Request(StarterRequest gsd)))

    | 0 , _ -> let bs' = status_end bs Blue in data.ba <- bs';
      if bs'.curr_hp <= 0 then faint_response data (rs, bs') else
      let gsd = data_to_gsd data in
      (None, gsd, Some (Request(StarterRequest gsd)), None)

    | _ , 0 -> let rs' = status_end rs Red in data.ra <- rs';
      if rs'.curr_hp <= 0 then faint_response data (rs', bs) else
      let gsd = data_to_gsd data in
      (None, gsd, None, Some (Request(StarterRequest gsd)))

    | _ , _ -> failwith "should not be reached" in

  (*assumes both steammon have not fainted yet. does end-of-turn status effects
    on both steammon and returns the appropriate game_output*)
  let alive_response data (rs, bs) : game_output =
    battle_error := false;
    let rs' = status_end rs Red and bs' = status_end bs Blue in
    data.ra <- rs';
    data.ba <- bs';
    add_update(UpdateSteammon (rs'.species, rs'.curr_hp, rs'.max_hp, Red));
    add_update(UpdateSteammon (bs'.species, bs'.curr_hp, bs'.max_hp, Blue));
    let gsd = data_to_gsd data in
    match rs'.curr_hp , bs'.curr_hp with
    | 0 , 0 -> (None, gsd, Some (Request(StarterRequest gsd)), 
        Some (Request(StarterRequest gsd)))
    | 0 , _ -> (None, gsd, Some (Request(StarterRequest gsd)), None)
    | _ , 0 -> (None, gsd, None, Some (Request(StarterRequest gsd)))
    | _ , _ -> (None, gsd, Some (Request(ActionRequest gsd)), 
        Some (Request(ActionRequest gsd))) in

  (*a set of preliminary actions performed during the battle phase before
    each step *)
  let pre_battle_phase () : unit =
    copy_game_to_data g data;
    first := first_action data.ra data.ba;
    add_update (SetFirstAttacker !first);
    data.ra <- status_recover data.ra Red;
    data.ba <- status_recover data.ba Blue;
    add_update(UpdateSteammon (data.ra.species, 
      data.ra.curr_hp, data.ra.max_hp, Red));
    add_update(UpdateSteammon (data.ba.species, 
      data.ba.curr_hp, data.ba.max_hp, Blue)) in

  (if !state = BattleSelect && (all_fainted (data.ra::data.rp) || 
    all_fainted (data.ba::data.bp)) then 
      state := Winner);
  (if !state = Battle && !battle_error = false then 
    pre_battle_phase ());

  match g, ra, ba with 
  | Game gsd, _ , _ when !state = Winner -> 
      state := Init;
      draftRD := 1;
      battle_error := false;
      let rsl = data.ra::data.rp and bsl = data.ba::data.bp in
      if all_fainted rsl && all_fainted bsl then (Some Tie, gsd, None, None)
      else if all_fainted rsl then (Some (Winner Blue), gsd, None, None)
      else (Some (Winner Red), gsd, None, None)
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) when 
    !state = Init ->
      initialize gsd rName bName
  | Game gsd, Action(SendTeamName (rName)), _ when !state = Init -> 
      initialize gsd rName "Blue"
  | Game gsd, _ , Action(SendTeamName (bName)) when !state = Init -> 
      initialize gsd "Red" bName

  | Game gsd, Action(PickSteammon rname), Action(PickSteammon bname) -> 
    if !draftColor = Red then 
      draft gsd rname 
    else draft gsd bname 
  | Game gsd, Action(PickSteammon name), _ when !state = Drafting -> 
    draft gsd name
  | Game gsd, _ , Action(PickSteammon name) when !state = Drafting -> 
    draft gsd name 

  | Game gsd, Action(PickInventory (invlst1)), Action(PickInventory (invlst2)) when 
    !state = Inventory ->
      inventory gsd invlst1 invlst2 false false 
  | Game gsd, Action(PickInventory(invlst1)), _ when !state = Inventory -> 
    inventory gsd invlst1 [] false true 
  | Game gsd, _ , Action(PickInventory(invlst2)) when !state = Inventory -> 
    inventory gsd [] invlst2 true false 

  (*battle phase*)
    (*first step: both players select their starting pokemon*)
  | Game gsd, Action(SelectStarter rs), Action(SelectStarter bs) when 
    !state = BattleSelect -> 
      copy_game_to_data (Game gsd) data;
      switch_in rs Red data; switch_in bs Blue data;
      state := Battle;
      alive_response data (data.ra, data.ba)
    
  | Game gsd, Action(UseMove rmove), Action(UseMove bmove) when 
    !state = Battle -> begin
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

  | Game gsd, Action(UseMove rmove), Action(UseItem(bi, bs_name)) when 
    !state = Battle -> begin
      match !first with
      | Red -> perform_move rmove (data.ra, data.ba) Red data;
          if data.ra.curr_hp <= 0 then begin
            use_item bi bs_name Blue data;
            faint_response data (data.ra, data.ba) end
          else if data.ba.curr_hp <= 0 then 
            faint_response data (data.ra, data.ba)
          else begin use_item bi bs_name Blue data;
            alive_response data (data.ra, data.ba) end
      | Blue -> use_item bi bs_name Blue data;
          perform_move rmove (data.ra, data.ba) Red data;
          if check_faint (data.ra, data.ba) then 
            faint_response data (data.ra, data.ba)
          else 
            alive_response data (data.ra, data.ba)
      end

  | Game gsd, Action(UseItem (ri, rs_name)), Action(UseMove bmove) when 
    !state = Battle -> begin
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
          else if data.ra.curr_hp <= 0 then 
            faint_response data (data.ra, data.ba)
          else begin use_item ri rs_name Red data;
            alive_response data (data.ra, data.ba) end
      end

  (*note: fix case where one mon faints and the other mon uses a self-move*)

  | Game gsd, Action(UseMove rmove), Action(SwitchSteammon bs_name) when 
    !state = Battle -> begin
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

  | Game gsd, Action(SwitchSteammon rs_name), Action(UseMove bmove) when 
    !state = Battle -> begin
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

  | Game gsd, Action(UseItem (ri, rs)), Action(UseItem (bi, bs)) when 
    !state = Battle -> begin
      match !first with
      | Red -> use_item ri rs Red data; use_item bi bs Blue data
      | Blue -> use_item bi bs Blue data; use_item ri rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action(SwitchSteammon rs), Action(SwitchSteammon bs) when 
    !state = Battle -> begin
      match !first with
      | Red -> switch_in rs Red data; switch_in bs Blue data
      | Blue -> switch_in bs Blue data; switch_in rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action(UseItem(ri, rs)), Action(SwitchSteammon bs) when 
    !state = Battle -> begin
      match !first with
      | Red -> use_item ri rs Red data; switch_in bs Blue data
      | Blue -> switch_in bs Blue data; use_item ri rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action(SwitchSteammon rs), Action(UseItem(bi, bs)) when 
    !state = Battle -> begin
      match !first with
      | Red -> switch_in rs Red data; use_item bi bs Blue data
      | Blue -> use_item bi bs Blue data; switch_in rs Red data
    end;
    alive_response data (data.ra, data.ba)

  | Game gsd, Action(UseMove rmove), _ when !state = Battle ->
      perform_move rmove (data.ra, data.ba) Red data;
      alive_response data (data.ra, data.ba)

  | Game gsd, _ , Action(UseMove bmove) when !state = Battle ->
      perform_move bmove (data.ra, data.ba) Blue data;
      alive_response data (data.ra, data.ba)

  | Game gsd, Action(SwitchSteammon rs), _ when !state = Battle ->
      switch_in rs Red data;
      alive_response data (data.ra, data.ba)

  | Game gsd, _ , Action(SwitchSteammon bs) when !state = Battle ->
      switch_in bs Blue data;
      alive_response data (data.ra, data.ba)

  | Game gsd, Action(UseItem(ri, rs)), _ when !state = Battle ->
      use_item ri rs Red data;
      alive_response data (data.ra, data.ba)

  | Game gsd, _ , Action(UseItem(bi, bs)) when !state = Battle ->
      use_item bi bs Red data;
      alive_response data (data.ra, data.ba)

  | Game gsd, _ , Action(SelectStarter bs) when !state = BattleSelect ->
      switch_in bs Blue data; state := Battle;
      alive_response data (data.ra, data.ba)
  | Game gsd, Action(SelectStarter rs), _  when !state = BattleSelect ->
      switch_in rs Red data; state := Battle;
      alive_response data (data.ra, data.ba)

  | Game gsd, DoNothing, DoNothing
  | Game gsd, _, _ -> exception_handle gsd

(* init_game generates a blank copy of the game, returning (gs * r1 * r2 
 * al * sl).
 * gs is a game with inventories initialized, and neither player with any
 *   steammon.
 * r1 is the first request sent out to the red team
 * r2 is the first request sent out to the blue team
 * al is the list of all attacks, as read from attack.txt
 * sl is the list of all steammon as read from steammon.txt
 *)
let init_game () =
  init_pool ("moves.csv") ("steammon.csv");
  (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
      TeamNameRequest,TeamNameRequest, 
      hash_to_list (Initialization.move_table), 
      hash_to_list(draftpool))  