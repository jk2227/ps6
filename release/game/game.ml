open Definitions
open Util
open Constants
open Netgraphics
open Initialization

(* You have to implement this. Change it from int to yout own state type*)
type game = Game of game_status_data

let draftRD = ref 1
let draftpool = Initialization.mon_table 

let game_datafication g = match g with 
  | Game x -> x 
	
let game_from_data game_data = Game (game_data)

let handle_step g ra ba = 
  
  (*TODO: Exception handling*)
  let draftupdate name s c=
    add_update(UpdateSteammon (name, s.curr_hp, s.max_hp, c)); 
    add_update(SetChosenSteammon (name));
    Table.remove draftpool name; 
    draftRD := !draftRD + 1; ()
  in

  let findMin name s = 
    Table.fold (fun k v (n,m) -> 
        if v.cost < m.cost then (k,v) else (n,m))
              draftpool (name,s) 

   in

  let draft color ((rsl,ri,rcred),(bsl,bi,bcred)) name = 
    if Table.mem draftpool name && (List.length rsl < 6 ||
      List.length bsl <6) then 
      let s = Table.find draftpool name in 
      let sCred = s.cost in 
      match color with 
      | Red when ((!draftRD mod 2 = 1) && (sCred <= rcred)) -> 
            draftupdate name s Red; 
            let gsd = (((s::rsl),ri,(rcred-sCred)), (bsl,bi,bcred)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Blue,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
      | Red when ((!draftRD mod 2 = 0) && (sCred <= rcred)) ->
            draftupdate name s Red;
            let gsd = (((s::rsl),ri,(rcred-sCred)), (bsl,bi,bcred)) in
            (None, gsd,Some(Request(PickRequest(Red,gsd,
            hash_to_list (Initialization.move_table), 
            hash_to_list(draftpool)))), None) 
      | Red when !draftRD mod 2  = 1-> let (n,m) = findMin name s in 
        draftupdate n m Red ;
        if m.cost > rcred then 
         let gsd = (((m::rsl),ri,0), (bsl,bi,bcred)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Blue,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
        else  
          let gsd = (((m::rsl),ri,(rcred-m.cost)), (bsl,bi,bcred)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Blue,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
      | Red -> let (n,m) = findMin name s in 
        draftupdate n m Red ;
        if m.cost > rcred then 
         let gsd = (((m::rsl),ri,0), (bsl,bi,bcred)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Red,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
        else  
          let gsd = (((m::rsl),ri,(rcred-m.cost)), (bsl,bi,bcred)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Red,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool)))))
      | Blue when ((!draftRD mod 2 = 1) && (sCred <= bcred)) ->  
            draftupdate name s Blue; 
            let gsd = ((rsl,ri,rcred), (s::bsl,bi,bcred-sCred)) in
            (None, gsd,Some(Request(PickRequest(Red,gsd,
            hash_to_list (Initialization.move_table), 
            hash_to_list(draftpool)))), None) 
      | Blue when ((!draftRD mod 2 = 0) && (sCred <= bcred))-> 
            draftupdate name s Blue;
            let gsd = ((rsl,ri,rcred), (s::bsl,bi,bcred-sCred)) in
            (None, gsd, None,
               Some(Request(PickRequest(Blue,gsd,
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
      | Blue when !draftRD mod 2  = 1-> let (n,m) = findMin name s in 
        draftupdate n m Blue;
        if m.cost > bcred then 
         let gsd = ((rsl,ri,rcred), (m::bsl,bi,0)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Red,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
        else  
          let gsd = ((rsl,ri,(rcred)), (m::bsl,bi,bcred-m.cost)) in
            (None, gsd, None, 
              Some(Request(PickRequest(Red,gsd, 
                hash_to_list (Initialization.move_table), 
                hash_to_list(draftpool))))) 
      | Blue -> let (n,m) = findMin name s in 
        draftupdate n m Blue;
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

    else if (List.length rsl = 6 && List.length bsl = 6) then 
      let gsd = ((rsl,ri,rcred),(bsl,bi,bcred)) in 
      (None, gsd, Some(Request(PickInventoryRequest gsd)), 
      Some(Request(PickInventoryRequest gsd))) 
    
    else failwith "meh"
  in

  let inventory ((rsl,ri,rcred),(bsl,bi,bcred)) invlstR invlstB = 
    let default = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE;
     cNUM_FULL_HEAL; cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in 
    let itemlst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_REVIVE;
    cCOST_FULLHEAL; cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in 
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
  in 

  let battle gsd rs bs = 
   (*I think faster one attacks first*) 
   (* add_update (SetFirstAttacker Red) ; *)
    failwith "TODO"
  in 

  match g, ra, ba with 
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) ->
    send_update (InitGraphics (rName,bName));
    if Random.int 2 = 0 then 
    (None, gsd, Some(Request(PickRequest(Red,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool)))), None)
    else (None, gsd, None, Some(Request(PickRequest(Blue,gsd,
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))))) 
  | Game gsd, Action(PickSteammon name), DoNothing -> draft Red gsd name
  | Game gsd, DoNothing, Action(PickSteammon name) -> draft Blue gsd name 
  | Game gsd, Action(PickInventory (invlst1)), Action(PickInventory (invlst2))->
   inventory gsd invlst1 invlst2 
  | Game gsd, Action(SelectStarter (rs)), Action(SelectStarter (bs)) -> 
    battle gsd rs bs 
  | _ -> failwith "swag"

 let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
        TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))