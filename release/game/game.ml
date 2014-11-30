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

let game_datafication g = match g with 
  | Game x -> x 
	
let game_from_data game_data = Game (game_data)

let handle_step g ra ba = 
  
  let changeColor () = match !draftColor with 
  | Red -> draftColor := Blue
  | Blue -> draftColor := Red
  in 

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
    begin 
      drafting := 0;
      invent := 1;
      let gsd = ((rsl,ri,rcred),(bsl,bi,bcred)) in 
      (None, gsd, Some(Request(PickInventoryRequest gsd)), 
      Some(Request(PickInventoryRequest gsd))) 
    end

    else failwith "meh"
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
        | Red -> if !draftRD mod 2 = 1 then changeColor ();  
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
        | Blue -> if !draftRD mod 2 = 1 then changeColor ();
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

  match g, ra, ba with 
  | Game gsd, Action(SendTeamName (rName)), Action (SendTeamName (bName)) ->
      initialize gsd rName bName
  | Game gsd, Action(SendTeamName (rName)), DoNothing -> 
      initialize gsd rName "Blue"
  | Game gsd, DoNothing, Action(SendTeamName (bName)) -> 
      initialize gsd "Red" bName
  | Game gsd, Action(PickSteammon name), DoNothing -> draft Red gsd name
  | Game gsd, DoNothing, Action(PickSteammon name) -> draft Blue gsd name 
  | Game gsd, Action(PickInventory (invlst1)), Action(PickInventory (invlst2))->
      inventory gsd invlst1 invlst2 false
  | Game gsd, Action(SelectStarter (rs)), Action(SelectStarter (bs)) -> 
      battle gsd rs bs 
  | Game gsd, DoNothing, DoNothing -> exception_handle gsd 
  | _ -> failwith "swag"

 let init_game () =
    init_pool ("moves.csv") ("steammon.csv");
    (Game(([],[],cSTEAMMON_CREDITS),([],[],cSTEAMMON_CREDITS)),
        TeamNameRequest,TeamNameRequest, 
        hash_to_list (Initialization.move_table), 
        hash_to_list(draftpool))
