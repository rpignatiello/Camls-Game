(* To run this file: OCAMLRUNPARAM=b dune exec test/main.exe *)
open OUnit2
open CamlGame
open Camel
open State
open Inputprocessor

let data_dir_prefix = "data" ^ Filename.dir_sep
let camelSetting = Yojson.Basic.from_file (data_dir_prefix ^ "camelSetting.json")
let state = Yojson.Basic.from_file (data_dir_prefix ^ "state.json")
let pp_string s = "\"" ^ s ^ "\""

let item_for_building_test (name : string) input1 (input2 : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (item_for_building (Camel.from_json input1) input2)
    ~printer:pp_string

let number_for_building_test (name : string) input1 (input2 : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (number_for_building (Camel.from_json input1) input2)
    ~printer:string_of_int

let produce_item_building_test (name : string) input1 (input2 : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (produce_item_building (Camel.from_json input1) input2)
    ~printer:pp_string

let production_rate_building_test (name : string) input1 (input2 : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (production_rate_building (Camel.from_json input1) input2)
    ~printer:string_of_float

let item_for_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      item_for_building (Camel.from_json input1) input2)

let number_for_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      number_for_building (Camel.from_json input1) input2)

let produce_item_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      produce_item_building (Camel.from_json input1) input2)

let production_rate_building_test_assert (name : string) input1
    (input2 : string) : test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      production_rate_building (Camel.from_json input1) input2)

let camel_tests =
  [
    item_for_building_test "item required to make building field" camelSetting
      "field" "catnip";
    item_for_building_test_assert "item not a valid building" camelSetting "cat";
    number_for_building_test "amount needed to build a building test"
      camelSetting "field" 10;
    number_for_building_test_assert
      "amount needed to build from invalid building" camelSetting "cat";
    produce_item_building_test "item produced from valid building" camelSetting
      "field" "catnip";
    produce_item_building_test_assert "item produced from invalid building"
      camelSetting "cat";
    production_rate_building_test "production rate of valid building"
      camelSetting "field" 0.125;
    production_rate_building_test_assert "production rate of invalid building"
      camelSetting "cat";
  ]

let quantity_of_building_test (name : string) input1 (input2 : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (quantity_of_building (State.from_json input1) input2)
    ~printer:string_of_int

let quantity_of_camel_test (name : string) input1 (expected_output : int) : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (quantity_of_camel (State.from_json input1))
    ~printer:string_of_int

let building_list_test (name : string) input1 (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (building_list (State.from_json input1))

let quantity_of_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (State.UnknownBuilding "Building Not Found") (fun () ->
      quantity_of_building (State.from_json input1) input2)

let tick_test (name : string) (state : t) (resource : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.get_resource (State.tick state) resource)

let buy_building_test (name : string) (state : t) (resource_to_edit : string)
    (amt : float) (building_type : string) (quantity : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (quantity_of_building
       (State.buy_building
          (State.edit_resource state resource_to_edit amt)
          quantity building_type)
       building_type)

let buy_building_new_resource_test (name : string) (state : t)
    (resource_to_edit : string) (amt : float) (building_type : string)
    (quantity : int) (resource_to_find : string) (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_resource
       (State.buy_building
          (State.edit_resource state resource_to_edit amt)
          quantity building_type)
       resource_to_find)

let buy_building_exception_test (name : string) (state : t) (quantity : int)
    (building_type : string) : test =
  name >:: fun _ ->
  assert_raises
    (State.NotEnoughMoney "You don't have enough money for this purchase.")
    (fun () -> State.buy_building state quantity building_type)

let game_state = from_json state

let state_tests =
  [
    quantity_of_building_test "quantity of valid building" state "field" 1;
    quantity_of_building_test_assert "quantity of invalid building" state "cat";
    quantity_of_camel_test "amount of camels user has at start" state 0;
    building_list_test "list of building" state [ "field" ];
    buy_building_test "buy building test" game_state "catnip" 100.0 "field" 10
      11;
    buy_building_test "buy logging test" game_state "catnip" 100.0 "logging" 1 1;
    buy_building_new_resource_test "buy building new resource test" game_state
      "catnip" 100.0 "logging" 1 "wood" 0.0;
    buy_building_exception_test "buy building exception test" game_state 10
      "field";
    tick_test "tick money test" game_state "catnip" 0.125;
    tick_test "tick loggingtest"
      (buy_building (State.edit_resource game_state "catnip" 10.0) 1 "logging")
      "wood" 0.125;
  ]

let parse_buy_test (name : string) (state : t) (input : string)
    (building : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.quantity_of_building
       (Inputprocessor.parse_input input
          (State.edit_resource game_state "catnip" 20.0))
       building)

let inputprocessor_tests =
  [ parse_buy_test "parse buy field test" game_state "buy field 2" "field" 3 ]

let suite =
  "test suite for Camel Project"
  >::: List.flatten [ camel_tests; state_tests; inputprocessor_tests ]

let _ = run_test_tt_main suite
