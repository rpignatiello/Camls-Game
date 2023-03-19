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
  production: float;
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
  settings = json |> member "setting" |> to_list |> List.map from_setting

}

let from_building json = {
  name = json |> member "name" |> to_string;
  cost = json |> member "cost" |> to_int;
  production = json |> member "production" |> to_float
}

let from_json json = {
  items = json |> member "items" |> to_list|> List.map from_item;
  buildings = json |> member "buildings" |> to_list |> List.map from_building
}