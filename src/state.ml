open Yojson.Basic.Util

exception UnknownBuilding of string

type building = {
  name : string;
  quantity : int;
}

type t = {
  camel : int;
  buildings : building list;
}

let from_building json =
  {
    name = json |> member "name" |> to_string;
    quantity = json |> member "quantity" |> to_int;
  }

let from_json json =
  {
    camel = json |> member "Camels" |> to_int;
    buildings = json |> member "Buildings" |> to_list |> List.map from_building;
  }

let quantity_of_building user building =
  match List.filter (fun x -> x.name = building) user.buildings with
  | [] -> raise (UnknownBuilding "Building Not Found")
  | h :: _ -> h.quantity

let quantity_of_camel user = user.camel
let building_list user = List.map (fun b -> b.name) user.buildings
