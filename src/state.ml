open Yojson.Basic.Util
open Camel

exception UnknownBuilding of string
exception NotEnoughMoney of string

type owned_buildings = {
  name : string;
  quantity : int;
}

type t = {
  money : float;
  camel : int;
  owned_buildings : owned_buildings list;
  game_settings : Camel.t;
}

let game_settings =
  let camelSetting =
    Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "camelSetting.json")
  in
  from_json camelSetting

let from_building json =
  {
    name = json |> member "name" |> to_string;
    quantity = json |> member "quantity" |> to_int;
  }

let from_json json =
  {
    money = json |> member "Catnip" |> to_float;
    camel = json |> member "Camels" |> to_int;
    owned_buildings =
      json |> member "Buildings" |> to_list |> List.map from_building;
    game_settings;
  }

let quantity_of_building user building =
  match List.filter (fun x -> x.name = building) user.owned_buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.quantity

let quantity_of_camel user = user.camel
let building_list user = List.map (fun b -> b.name) user.owned_buildings
let money user = user.money

let edit_money state amt =
  {
    money = state.money +. amt;
    camel = state.camel;
    owned_buildings = state.owned_buildings;
    game_settings = state.game_settings;
  }

(** Increases the quantity of buildings for the player and decreases by the
    corresponding cost*)
let buy_building state (quantity : int) (building_type : string) =
  let total_cost =
    float_of_int (number_for_building state.game_settings building_type)
  in
  if total_cost > state.money then
    raise (NotEnoughMoney "You don't have enough money for this purchase.")
  else
    let new_owned_buildings =
      List.map
        (fun b ->
          if b.name = building_type then
            { name = b.name; quantity = b.quantity + quantity }
          else b)
        state.owned_buildings
    in
    {
      money = state.money -. total_cost;
      camel = state.camel;
      owned_buildings = new_owned_buildings;
      game_settings = state.game_settings;
    }

(** Updates the amount of money the player has *)
let tick state =
  let money_producing_buildings =
    List.filter
      (fun b -> produce_item_building state.game_settings b.name = "catnip")
      state.owned_buildings
  in
  let money_sum =
    List.fold_right
      (fun b c ->
        float_of_int b.quantity
        *. production_rate_building state.game_settings b.name
        +. c)
      money_producing_buildings 0.
  in
  {
    money = money_sum;
    camel = state.camel;
    owned_buildings = state.owned_buildings;
    game_settings = state.game_settings;
  }
