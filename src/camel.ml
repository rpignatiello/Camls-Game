(*
  JSON file for save data
    read from file
    write to file

  village
    catnip, wood + other resources
    buildings that go in village
    camels

    purchase buildings
    update catnip... values

  
  buildings
    cost to purchase
    production
    quantity

  https://docs.python.org/3/howto/curses.html
*)
open Yojson.Basic.Util

exception UnknownBuilding of string

type setting = {
  required: int;
  required_item: string;
}

type item = {
  name: string;
  settings: setting list;
}

type building= {
  name: string;
  cost: int;
  cost_item : string;
  production: float;
  production_item: string
}

type t = {
  items: item list;
  buildings: building list;
}

let from_setting json = {
  required = json |> member "required" |> to_int;
  required_item = json |> member "required item" |> to_string;
}
let from_item json = {
  name = json |> member "name" |> to_string;
  settings = json |> member "setting" |> to_list |> List.map from_setting;
}

let from_building json = {
  name = json |> member "name" |> to_string;
  cost = json |> member "cost" |> to_int;
  cost_item = json |> member "cost item" |> to_string;
  production = json |> member "production" |> to_float;
  production_item = json |> member "production item" |> to_string
}

let from_json json = {
  items = json |> member "items" |> to_list|> List.map from_item;
  buildings = json |> member "buildings" |> to_list |> List.map from_building
}

let item_for_building setting building = match List.filter (fun x -> x.name = building) setting.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h::_ -> h.cost_item

let number_for_building setting building = match List.filter(fun x -> x.name = building) setting.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h::_ -> h.cost

let produce_item_building setting building = match List.filter (fun x -> x.name = building) setting.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.production_item

let production_rate_building setting building = match List.filter (fun x -> x.name = building) setting.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.production

let contains_building setting building = match List.filter (fun b -> b.name = building) setting.buildings with 
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> true 