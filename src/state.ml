open Yojson.Basic.Util
open Camel

exception UnknownBuilding of string
exception NotEnoughMoney of string
exception UnknownResource of string

type owned_buildings = {
  name : string;
  quantity : int;
  cost_now : float;
}

type resources = {
  name : string;
  quantity : float;
}

type t = {
  resources : resources list;
  camel : int;
  owned_buildings : owned_buildings list;
  game_settings : Camel.t;
  season : string;
  day : int;
}

let game_settings =
  let camelSetting =
    Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "camelSetting.json")
  in
  from_json camelSetting

let from_building json : owned_buildings =
  {
    name = json |> member "name" |> to_string;
    quantity = json |> member "quantity" |> to_int;
    cost_now = json |> member "cost now" |> to_float;
  }

let from_resources json =
  {
    name = json |> member "name" |> to_string;
    quantity = json |> member "quantity" |> to_float;
  }

let from_json json =
  {
    resources = json |> member "Resources" |> to_list |> List.map from_resources;
    camel = json |> member "Camels" |> to_int;
    owned_buildings =
      json |> member "Buildings" |> to_list |> List.map from_building;
    game_settings;
    season = json |> member "Season" |> to_string;
    day = json |> member "Day" |> to_int;
  }

let quantity_of_building user building =
  match
    List.filter
      (fun (x : owned_buildings) -> x.name = building)
      user.owned_buildings
  with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.quantity

let get_season user = user.season
let get_day user = user.day
let quantity_of_camel user = user.camel
let game_settings state = state.game_settings

let building_list user =
  List.map (fun (b : owned_buildings) -> b.name) user.owned_buildings

let resource_list user = List.map (fun (r : resources) -> r.name) user.resources
let calculate_camels user = quantity_of_building user "hut" * 2

let get_building user building =
  match
    List.filter
      (fun (b : owned_buildings) -> b.name = building)
      user.owned_buildings
  with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: t -> h

let cost user building = (get_building user building).cost_now

let rec add_building_type (buildings : owned_buildings list) (bldg : string)
    setting =
  match buildings with
  | h :: t ->
      if h.name = bldg then h :: t else h :: add_building_type t bldg setting
  | [] ->
      [
        {
          name = bldg;
          quantity = 0;
          cost_now = Camel.number_for_building setting bldg;
        };
      ]

let rec get_resource state resource =
  match List.filter (fun x -> x.name = resource) state.resources with
  | [] -> raise (UnknownResource "Resource Not Found")
  | h :: _ -> h.quantity

let rec add_resource_type (resources : resources list) (res : string) =
  match resources with
  | h :: t -> if h.name = res then h :: t else h :: add_resource_type t res
  | [] -> [ { name = res; quantity = 0.0 } ]

let edit_resource state resource amt =
  let resources_with_new_type = add_resource_type state.resources resource in
  let new_resources =
    List.map
      (fun b ->
        if b.name = resource then
          { name = b.name; quantity = b.quantity +. amt }
        else b)
      resources_with_new_type
  in
  {
    resources = new_resources;
    camel = calculate_camels state;
    owned_buildings = state.owned_buildings;
    game_settings = state.game_settings;
    season = state.season;
    day = state.day;
  }

let add_camelnip user =
  let new_resource =
    List.map
      (fun r ->
        if r.name = "camelnip" then
          { name = r.name; quantity = r.quantity +. 1.0 }
        else r)
      user.resources
  in
  {
    resources = new_resource;
    camel = user.camel;
    owned_buildings = user.owned_buildings;
    game_settings = user.game_settings;
    season = user.season;
    day = user.day;
  }

let trade state resource quantity =
  (* obtain total cost of purchase and amount of resources the use has *)
  let cost =
    float_of_int quantity
    *. resource_cost (resource_settings state.game_settings resource)
  in
  let required_resource =
    resource_cost_type (resource_settings state.game_settings resource)
  in
  let user_resource_amt = get_resource state required_resource in
  if cost > user_resource_amt then
    raise (NotEnoughMoney "You don't have enough money for this purchase.")
  else
    (* increase the purchased resource and decrease the payment resource*)
    let new_resources =
      List.map
        (fun r ->
          if r.name = resource then
            { name = r.name; quantity = r.quantity +. float_of_int quantity }
          else if r.name = required_resource then
            { name = r.name; quantity = r.quantity -. cost }
          else r)
        (add_resource_type state.resources resource)
    in
    {
      resources = new_resources;
      camel = calculate_camels state;
      owned_buildings = state.owned_buildings;
      game_settings = state.game_settings;
      season = state.season;
      day = state.day;
    }

(** Increases the quantity of buildings for the player and decreases resources
    by the corresponding cost*)
let buy_building state (quantity : int) (building_type : string) =
  if contains_building state.game_settings building_type then
    let total_cost =
      (get_building state building_type).cost_now *. float_of_int quantity
    in
    let resource_type = item_for_building state.game_settings building_type in
    let player_resource_amount =
      (List.nth
         (List.filter
            (fun r -> r.name = resource_type)
            (add_resource_type state.resources resource_type))
         0)
        .quantity
    in
    if total_cost > player_resource_amount then
      raise (NotEnoughMoney "You don't have enough money for this purchase.")
    else
      let new_resources =
        List.map
          (fun r ->
            if r.name = resource_type then
              { name = r.name; quantity = r.quantity -. total_cost }
            else r)
          (add_resource_type state.resources
             (produce_item_building state.game_settings building_type))
      in
      let new_owned_buildings =
        if Camel.contains_building state.game_settings building_type then
          List.map
            (fun (b : owned_buildings) : owned_buildings ->
              if b.name = building_type then
                {
                  name = b.name;
                  quantity = b.quantity + quantity;
                  cost_now =
                    b.cost_now
                    *. Camel.cost_multiplier state.game_settings b.name;
                }
              else b)
            (add_building_type state.owned_buildings building_type
               state.game_settings)
        else state.owned_buildings
      in
      {
        resources = new_resources;
        camel = calculate_camels state;
        owned_buildings = new_owned_buildings;
        game_settings = state.game_settings;
        season = state.season;
        day = state.day;
      }
  else raise (UnknownBuilding "Building Not Found")

let rec tick_helper (state : t) (resources : resources list)
    (buildings : owned_buildings list) =
  match buildings with
  | [] -> resources
  | h :: t ->
      let amt =
        float_of_int h.quantity
        *. production_rate_building state.game_settings h.name
        *. season_multiplier state.game_settings state.season
      in
      let new_resource_list =
        List.map
          (fun r ->
            if r.name = produce_item_building state.game_settings h.name then
              { name = r.name; quantity = r.quantity +. amt }
            else r)
          resources
      in
      tick_helper state new_resource_list t

let rec camelnip_consume resource_list camel hard =
  if hard then
    match resource_list with
    | [] -> []
    | h :: t ->
        if h.name = "camelnip" then
          {
            name = h.name;
            quantity = h.quantity -. (float_of_int camel *. 0.85);
          }
          :: t
        else h :: camelnip_consume t camel hard
  else resource_list

let tick (state : t) =
  let new_resource_list =
    tick_helper state state.resources state.owned_buildings
  in
  if state.day >= 999 then
    {
      resources =
        camelnip_consume new_resource_list (calculate_camels state) true;
      camel = calculate_camels state;
      owned_buildings = state.owned_buildings;
      game_settings = state.game_settings;
      season = next_season state.game_settings state.season;
      day = 1;
    }
  else
    {
      resources =
        camelnip_consume new_resource_list (calculate_camels state)
          (state.day > 200);
      camel = calculate_camels state;
      owned_buildings = state.owned_buildings;
      game_settings = state.game_settings;
      season = state.season;
      day = state.day + 1;
    }

let convert_buil_list (state : t) =
  String.concat ", "
    (List.fold_left
       (fun a x ->
         ("{ \n" ^ "\"name\": \"" ^ x ^ "\", \n" ^ "\"quantity\": "
         ^ string_of_int (quantity_of_building state x)
         ^ ",\n\"cost now\": "
         ^ string_of_float (cost state x)
         ^ "0\n}")
         :: a)
       [] (building_list state))

let convert_resources_lis (state : t) =
  String.concat ", "
    (List.fold_left
       (fun a x ->
         ("{ \n" ^ "\"name\": \"" ^ x ^ "\", \n" ^ "\"quantity\": "
         ^ string_of_float (get_resource state x)
         ^ "0\n" ^ "}")
         :: a)
       [] (resource_list state))

let save (state : t) =
  let input =
    "{ \n" ^ "\"Camels\": " ^ string_of_int state.camel ^ ", \n"
    ^ "\"Buildings\": [\n" ^ convert_buil_list state
    ^ "\n], \n \"Resources\": [\n"
    ^ convert_resources_lis state
    ^ "\n ],\n \"Season\": \"" ^ state.season ^ "\",\n \"Day\": "
    ^ string_of_int state.day ^ "\n }"
  in
  let oc = open_out ("data" ^ Filename.dir_sep ^ "state.json") in
  flush oc;
  Printf.fprintf oc "%s" input;
  flush oc;
  close_out oc
