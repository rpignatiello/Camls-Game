open OUnit2
open CamlGame
open Camel

let data_dir_prefix = "data" ^ Filename.dir_sep
let lonely = Yojson.Basic.from_file (data_dir_prefix ^ "camelSetting.json")