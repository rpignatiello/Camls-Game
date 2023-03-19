open OUnit2
open CamlGame
open Camel

let data_dir_prefix = "data" ^ Filename.dir_sep
let camelSetting = Yojson.Basic.from_file (data_dir_prefix ^ "camelSetting.json")
<<<<<<< HEAD
let pp_string s = "\"" ^ s ^ "\""

let item_for_building_test (name : string) input1 (input2 : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (item_for_building (from_json input1) input2)
    ~printer:pp_string

let item_for_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (UnknownBuilding "Building Not Found") (fun () ->
      item_for_building (from_json input1) input2)

let item_for_building_tests =
  [
    item_for_building_test "item required to make building field" camelSetting
      "field" "catnip";
    item_for_building_test_assert "item not a valid building" camelSetting
      "camel";
  ]

let suite =
  "test suite for Camel Project" >::: List.flatten [ item_for_building_tests ]

let _ = run_test_tt_main suite
=======

let item_for_building_test (name : string) (input1 : Camel.t) (input2: string) 
(expected_output : string) : test = 
>>>>>>> ec43700e048fc9cb4d85a09040872bc517926f4f
