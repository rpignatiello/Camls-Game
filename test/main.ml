open OUnit2
open CamlGame
open Camel

let data_dir_prefix = "data" ^ Filename.dir_sep
let camelSetting = Yojson.Basic.from_file (data_dir_prefix ^ "camelSetting.json")

let item_for_building_test (name : string) (input1 : Camel.t) (input2: string) 
(expected_output : string) : test = 
