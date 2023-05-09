open Yojson.Basic.Util
open Camel

exception UnknownBuilding of string
exception NotEnoughMoney of string
exception UnknownResource of string

type owned_buildings = {
  name : string;
  quantity : int;
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
  }

let quantity_of_building user building =
  match
    List.filter
      (fun (x : owned_buildings) -> x.name = building)
      user.owned_buildings
  with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.quantity

let quantity_of_camel user = user.camel
let game_settings state = state.game_settings

let building_list user =
  List.map (fun (b : owned_buildings) -> b.name) user.owned_buildings

let rec add_building_type (buildings : owned_buildings list) (bldg : string) =
  match buildings with
  | h :: t -> if h.name = bldg then h :: t else h :: add_building_type t bldg
  | [] -> [ { name = bldg; quantity = 0 } ]

let rec get_resource state resource =
  match List.filter (fun x -> x.name = resource) state.resources with
  | [] -> raise (UnknownResource "Resource Not Found")
  | h :: _ -> h.quantity

let rec add_resource_type (resources : resources list) (res : string) =
  match resources with
  | h :: t -> if h.name = res then h :: t else h :: add_resource_type t res
  | [] -> [ { name = res; quantity = 0.0 } ]

let edit_resource state resource amt =
  let new_resources =
    List.map
      (fun b ->
        if b.name = resource then
          { name = b.name; quantity = b.quantity +. amt }
        else b)
      state.resources
  in
  {
    resources = new_resources;
    camel = state.camel;
    owned_buildings = state.owned_buildings;
    game_settings = state.game_settings;
  }

(** Increases the quantity of buildings for the player and decreases resources
    by the corresponding cost*)
let buy_building state (quantity : int) (building_type : string) =
  if contains_building state.game_settings building_type then
    let total_cost =
      float_of_int (number_for_building state.game_settings building_type)
      *. float_of_int quantity
    in
    let resource_type = item_for_building state.game_settings building_type in
    let player_resource_amount =
      (List.nth
         (List.filter (fun r -> r.name = resource_type) state.resources)
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
        List.map
          (fun (b : owned_buildings) : owned_buildings ->
            if b.name = building_type then
              { name = b.name; quantity = b.quantity + quantity }
            else b)
          (add_building_type state.owned_buildings building_type)
      in
      {
        resources = new_resources;
        camel = state.camel;
        owned_buildings = new_owned_buildings;
        game_settings = state.game_settings;
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

let tick (state : t) =
  let new_resource_list =
    tick_helper state state.resources state.owned_buildings
  in
  {
    resources = new_resource_list;
    camel = state.camel;
    owned_buildings = state.owned_buildings;
    game_settings = state.game_settings;
  }
