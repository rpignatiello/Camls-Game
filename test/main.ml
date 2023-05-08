(* To run this file: OCAMLRUNPARAM=b dune exec test/main.exe *)
open OUnit2
open CamlGame
open Camel
open State

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

let tick_money_test (name : string) (state : t) (expected_output : float) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (State.money (State.tick state))

let buy_building_test (name : string) (money : float) (state : t)
    (quantity : int) (building_type : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (quantity_of_building
       (State.buy_building
          (State.edit_money state money)
          quantity building_type)
       building_type)

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
    tick_money_test "tick money test" game_state 0.125;
    buy_building_test "buy building test" 100. game_state 10 "field" 11;
    buy_building_exception_test "buy building exception test" game_state 10
      "field";
  ]

let suite =
  "test suite for Camel Project" >::: List.flatten [ camel_tests; state_tests ]

let _ = run_test_tt_main suite
