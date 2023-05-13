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
  
  seasons
    4 different season
    multiplier

  https://docs.python.org/3/howto/curses.html
*)
open Yojson.Basic.Util

exception UnknownBuilding of string
exception UnknownResource of string
exception UnknownSeason of string

type setting = {
  required: float;
  required_item: string;
}

type item = {
  name: string;
  settings: setting list;
}

type building = {
  name: string;
  cost: float;
  cost_item : string;
  production: float;
  production_item: string
}

type season = {
  seasonName : string;
  multiplier: float;
  next : string;
}

type t = {
  items: item list;
  buildings: building list;
  seasons : season list;
}

let from_setting json = {
  required = json |> member "required" |> to_float;
  required_item = json |> member "required item" |> to_string;
}
let from_item json = {
  name = json |> member "name" |> to_string;
  settings = json |> member "setting" |> to_list |> List.map from_setting;
}

let from_building json = {
  name = json |> member "name" |> to_string;
  cost = json |> member "cost" |> to_float;
  cost_item = json |> member "cost item" |> to_string;
  production = json |> member "production" |> to_float;
  production_item = json |> member "production item" |> to_string;
}

let from_season json = {
  seasonName = json |> member "name" |> to_string;
  multiplier = json |> member "multiplier" |> to_float;
  next = json |> member "next" |> to_string;
}

let from_json json = {
  items = json |> member "items" |> to_list|> List.map from_item;
  buildings = json |> member "buildings" |> to_list |> List.map from_building;
  seasons = json |> member "seasons" |> to_list |> List.map from_season;
}

let item_for_building setting building = match List.filter 
  (fun x -> x.name = building) setting.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h::_ -> h.cost_item

let number_for_building setting building = match List.filter 
  (fun x -> x.name = building) setting.buildings with
    | [] -> raise (UnknownBuilding "Building Not Found")
    | h::_ -> h.cost

let produce_item_building setting building = match List.filter 
  (fun x -> x.name = building) setting.buildings with
    | [] -> raise (UnknownBuilding "Building Not Found")
    | h :: _ -> h.production_item

let production_rate_building setting building = match List.filter 
  (fun x -> x.name = building) setting.buildings with
    | [] -> raise (UnknownBuilding "Building Not Found")
    | h :: _ -> h.production

let contains_building setting building = match List.filter 
  (fun b -> b.name = building) setting.buildings with 
    | [] -> raise (UnknownBuilding "Building Not Found")
    | h :: _ -> true 

let contains_resource setting resource = match 
  List.filter (fun (r : item) -> r.name = resource) setting.items with 
    | [] -> raise (UnknownResource "Resource Not Found")
    | h :: _ -> true  

let resource_settings setting resource = match 
  List.filter (fun (r : item) -> r.name = resource) setting.items with 
    | [] -> raise (UnknownResource "Resource Not Found")
    | h :: _ -> List.nth h.settings 0

let season_multiplier setting season = match 
  List.filter (fun (s : season) -> s.seasonName = season) setting.seasons with
    | [] -> raise (UnknownSeason "Season Not Found")
    | h :: _ -> h.multiplier

let next_season setting season = match 
  List.filter (fun (s : season) -> s.seasonName = season) setting.seasons with
    | [] -> raise (UnknownSeason "Season Not Found")
    | h :: _ -> h.next

let resource_cost setting = setting.required
let resource_cost_type setting = setting.required_item