(* Our testing strategy was to test all public functions. If a function is
   called from another file, it would be tested here. We intentionally did not
   test the frontend/front.ml as the frontend operates very differently from the
   other components of the project.

   All three of our main modules were tested using OUnit. We implemented glass
   box testing when writing test cases. Some test cases involve altering the
   state of the game to change resource amounts to be able to purchase different
   buildings or trade for different resources. Glass box testing is also
   utilized to ensure that the correct exceptions get thrown.

   Our testing shows the correctness of our system. We cover a variety of cases
   and in combination with the use of our front end interface we can ensure that
   the game behaves as expected. *)
open OUnit2
open CamlGame
open Camel
open State
open Inputprocessor

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (number_for_building (Camel.from_json input1) input2)
    ~printer:string_of_float

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

let contains_building_test (name : string) input1 (input2 : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (contains_building (Camel.from_json input1) input2)
    ~printer:string_of_bool

let contains_resource_test (name : string) input1 (input2 : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (contains_resource (Camel.from_json input1) input2)
    ~printer:string_of_bool

let resource_cost_test (name : string) input1 (input2 : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (resource_cost (resource_settings (Camel.from_json input1) input2))
    ~printer:string_of_float

let resource_cost_type_test (name : string) input1 (input2 : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (resource_cost_type (resource_settings (Camel.from_json input1) input2))
    ~printer:pp_string

let season_multiplier_test (name : string) input1 (input2 : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (season_multiplier (Camel.from_json input1) input2)
    ~printer:string_of_float

let next_season_test (name : string) input1 (input2 : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (next_season (Camel.from_json input1) input2)
    ~printer:pp_string

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

let contains_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      contains_building (Camel.from_json input1) input2)

let contains_resource_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownResource "Resource Not Found") (fun () ->
      contains_resource (Camel.from_json input1) input2)

let resource_cost_test_assert (name : string) input1 (input2 : string) : test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownResource "Resource Not Found") (fun () ->
      resource_cost (resource_settings (Camel.from_json input1) input2))

let resource_cost_type_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownResource "Resource Not Found") (fun () ->
      resource_cost_type (resource_settings (Camel.from_json input1) input2))

let season_multiplier_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownSeason "Season Not Found") (fun () ->
      season_multiplier (Camel.from_json input1) input2)

let next_season_test_assert (name : string) input1 (input2 : string) : test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownSeason "Season Not Found") (fun () ->
      next_season (Camel.from_json input1) input2)

let camel_tests =
  [
    item_for_building_test "item required to make building field" camelSetting
      "field" "camelnip";
    item_for_building_test_assert "item not a valid building" camelSetting "cat";
    number_for_building_test "amount needed to build a building test"
      camelSetting "field" 10.;
    number_for_building_test_assert
      "amount needed to build from invalid building" camelSetting "cat";
    produce_item_building_test "item produced from valid building" camelSetting
      "field" "camelnip";
    produce_item_building_test_assert "item produced from invalid building"
      camelSetting "cat";
    production_rate_building_test "production rate of valid building"
      camelSetting "field" 0.125;
    production_rate_building_test_assert "production rate of invalid building"
      camelSetting "cat";
    contains_building_test "valid building" camelSetting "field" true;
    contains_building_test_assert "invalid building" camelSetting "cat";
    contains_resource_test "valid resource" camelSetting "camelnip" true;
    contains_resource_test_assert "invalid resource" camelSetting "cat";
    resource_cost_test "valid resource cost camelnip" camelSetting "camelnip"
      0.0;
    resource_cost_test "valid resource cost wood" camelSetting "wood" 100.0;
    resource_cost_test_assert "invalid resource cost" camelSetting "cat";
    resource_cost_type_test "valid resource required item camelnip" camelSetting
      "camelnip" "camelnip";
    resource_cost_type_test "valid resource required item wood" camelSetting
      "wood" "camelnip";
    resource_cost_type_test_assert "invalid resource" camelSetting "cat";
    season_multiplier_test "valid season spring" camelSetting "spring" 1.5;
    season_multiplier_test "valid season summer" camelSetting "summer" 1.0;
    season_multiplier_test "valid season fall" camelSetting "fall" 1.0;
    season_multiplier_test "valid season winter" camelSetting "winter" 0.25;
    season_multiplier_test_assert "invalid season" camelSetting "autumn";
    next_season_test "valid season spring" camelSetting "spring" "summer";
    next_season_test "valid season summer" camelSetting "summer" "fall";
    next_season_test "valid season fall" camelSetting "fall" "winter";
    next_season_test "valid season winter" camelSetting "winter" "spring";
    next_season_test_assert "invalid season" camelSetting "autumn";
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
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (building_list (State.from_json input1))

let resource_list_test (name : string) input1 (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (resource_list (State.from_json input1))

let calculate_camels_test (name : string) input1 (expected_output : int) : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (calculate_camels (State.from_json input1))
    ~printer:string_of_int

let quantity_of_building_test_assert (name : string) input1 (input2 : string) :
    test =
  name >:: fun _ ->
  assert_raises (State.UnknownBuilding "Building Not Found") (fun () ->
      quantity_of_building (State.from_json input1) input2)

let tick_test (name : string) (state : t) (resource : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.get_resource (State.tick state) resource)
    ~printer:string_of_float

let get_season_test (name : string) (user : t) (expected_output : string) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (State.get_season user) ~printer:pp_string

let get_day_test (name : string) (user : t) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.get_day user) ~printer:string_of_int

let cost_test (name : string) (user : t) (building : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.cost user building)
    ~printer:string_of_float

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
    ~printer:string_of_int

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

let save_test (name : string) (state : t) =
  name >:: fun _ ->
  assert (
    State.save state;
    true)

let trade_test (name : string) (state : t) (resource_to_edit : string)
    (amt : float) (resource_type : string) (quantity : int)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.get_resource
       (State.trade
          (State.edit_resource state resource_to_edit amt)
          resource_type quantity)
       resource_type)
    ~printer:string_of_float

let convert_buil_list_test (name : string) (user : t) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (State.convert_buil_list user) ~printer:pp_string

let convert_resources_list_test (name : string) (user : t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.convert_buil_list user) ~printer:pp_string

let game_state = from_json state

let state_tests =
  [
    quantity_of_building_test "quantity of valid building" state "field" 1;
    quantity_of_building_test_assert "quantity of invalid building" state "cat";
    quantity_of_camel_test "amount of camels user has at start" state 2;
    building_list_test "list of building" state [ "field"; "hut" ];
    resource_list_test "list of resources" state [ "camelnip" ];
    buy_building_test "buy building test" game_state "camelnip" 100.0 "field" 10
      11;
    buy_building_test "buy logging test" game_state "camelnip" 100.0 "logging" 1
      1;
    buy_building_new_resource_test "buy building new resource test" game_state
      "camelnip" 100.0 "logging" 1 "wood" 0.0;
    buy_building_exception_test "buy building exception test" game_state 10
      "field";
    tick_test "tick money test" game_state "camelnip" 0.125;
    tick_test "tick logging test"
      (buy_building
         (State.edit_resource game_state "camelnip" 10.0)
         1 "logging")
      "wood" 0.125;
    trade_test "trade test" game_state "camelnip" 1000.0 "wood" 10 10.0;
    get_season_test "current season" game_state "summer";
    calculate_camels_test "camels at start" state 2;
    convert_buil_list_test "string version of original state" game_state
      ("{ \n" ^ "\"name\": \"" ^ "hut" ^ "\", \n" ^ "\"quantity\": "
     ^ string_of_int 1 ^ ",\n" ^ "\"cost now\": " ^ "5.0" ^ "\n" ^ "}" ^ ", "
     ^ "{ \n" ^ "\"name\": \"" ^ "field" ^ "\", \n" ^ "\"quantity\": "
     ^ string_of_int 1 ^ ",\n" ^ "\"cost now\": " ^ "10.0" ^ "\n" ^ "}");
    convert_resources_list_test "string version of original state resources"
      game_state
      ("{ \n" ^ "\"name\": \"" ^ "hut" ^ "\", \n" ^ "\"quantity\": "
     ^ string_of_int 1 ^ ",\n" ^ "\"cost now\": " ^ "5.0" ^ "\n" ^ "}" ^ ", "
     ^ "{ \n" ^ "\"name\": \"" ^ "field" ^ "\", \n" ^ "\"quantity\": "
     ^ string_of_int 1 ^ ",\n" ^ "\"cost now\": " ^ "10.0" ^ "\n" ^ "}");
    cost_test "cost of field at start" game_state "field" 10.0;
    cost_test "cost of hut at start" game_state "hut" 5.0;
    get_day_test "day at start" game_state 1;
    buy_building_test "buy hut test" game_state "wood" 10.0 "hut" 1 2;
    buy_building_exception_test "buy hut exception test" game_state 10 "hut";
  ]

let parse_buy_test (name : string) (state : t) (input : string)
    (building : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.quantity_of_building
       (Inputprocessor.parse_input input
          (State.edit_resource game_state "camelnip" 20.0))
       building)

let parse_trade_test (name : string) (state : t) (input : string)
    (resource : string) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.get_resource
       (Inputprocessor.parse_input input
          (State.edit_resource game_state "camelnip" 1000.0))
       resource)

let parse_buy_num_args_exc_test (name : string) (state : t) (input : string) :
    test =
  name >:: fun _ ->
  assert_raises
    (Inputprocessor.InvalidInput
       "Error: Incorrect Number of Arguments Provided. To buy a building use \
        the form [buy] [building] [quantity]") (fun () ->
      Inputprocessor.parse_input input state)

let parse_buy_bldg_not_found_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownBuilding "Building Not Found") (fun () ->
      Inputprocessor.parse_input input state)

let parse_buy_invalid_quant_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises
    (Inputprocessor.InvalidInput "Invalid Quantity to Purchase Entered.")
    (fun () -> Inputprocessor.parse_input input state)

let parse_buy_quantity_too_small_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises
    (Inputprocessor.InvalidInput
       "Error: Quantity to Purchase Must be Greater than 0.") (fun () ->
      Inputprocessor.parse_input input state)

let parse_bonfire_inv_quant_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises
    (Inputprocessor.InvalidInput
       "Error: Incorrect Number of Arguments Provided. To gather camelnip, \
        type [gather]") (fun () -> Inputprocessor.parse_input input state)

let parse_gather_test (name : string) (state : t) (input : string)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_resource (Inputprocessor.parse_input input state) "camelnip")

let parse_trade_inv_quant_args_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises
    (Inputprocessor.InvalidInput
       "Error: Incorrect Number of Arguments Provided. To trade resources use \
        the form [trade] [target resource] [quantity]") (fun () ->
      Inputprocessor.parse_input input state)

let parse_trade_res_not_found (name : string) (state : t) (input : string) :
    test =
  name >:: fun _ ->
  assert_raises (Camel.UnknownResource "Resource Not Found") (fun () ->
      Inputprocessor.parse_input input state)

let parse_trade_inv_quant_exc_test (name : string) (state : t) (input : string)
    : test =
  name >:: fun _ ->
  assert_raises (InvalidInput "Invalid Quantity to Trade for Entered")
    (fun () -> Inputprocessor.parse_input input state)

let parse_trade_quant_too_small_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises
    (InvalidInput "Error: Quantity to Trade for Must be Greater than 0.")
    (fun () -> Inputprocessor.parse_input input state)

let parse_save_inv_arg_num_exc_test (name : string) (state : t) (input : string)
    : test =
  name >:: fun _ ->
  assert_raises (InvalidInput "To save game data, type [save]") (fun () ->
      Inputprocessor.parse_input input state)

let parse_reset_inv_arg_num_exc_test (name : string) (state : t)
    (input : string) : test =
  name >:: fun _ ->
  assert_raises (InvalidInput "To reset save data, type [reset]") (fun () ->
      Inputprocessor.parse_input input state)

let parse_unknown_command_exc_test (name : string) (state : t) (input : string)
    : test =
  name >:: fun _ ->
  assert_raises (InvalidInput "Error: Invalid Input. Unknown Command")
    (fun () -> Inputprocessor.parse_input input state)

let inputprocessor_tests =
  [
    parse_buy_test "parse buy field test" game_state "buy field 2" "field" 3;
    parse_trade_test "parse trade wood test" game_state "trade wood 10" "wood"
      10.0;
    parse_buy_num_args_exc_test "parse buy incorrect num args exc test"
      game_state "buy wood";
    parse_buy_bldg_not_found_exc_test "parse buy bldg not found exc test"
      game_state "buy farms 1";
    parse_buy_invalid_quant_exc_test "parse buy invalid quantity exc test"
      game_state "buy field five";
    parse_buy_quantity_too_small_exc_test
      "parse buy quantity too small exc test" game_state "buy field 0";
    parse_bonfire_inv_quant_exc_test
      "parse invalid arg quantity bonfire exc test" game_state "gather 1";
    parse_trade_inv_quant_args_exc_test "parse trade invalid arg quant test"
      game_state "trade wood";
    parse_trade_res_not_found "parse trade resource not found exc test"
      game_state "trade logs 1";
    parse_trade_inv_quant_exc_test "parse trade invalid quant trade exc test"
      game_state "trade wood one";
    parse_trade_quant_too_small_exc_test "parse trade quant too small exc test"
      game_state "trade wood -1";
    parse_trade_quant_too_small_exc_test "parse trade quant too small exc test"
      game_state "trade wood 0";
    parse_save_inv_arg_num_exc_test "parse save inv arg num exc test" game_state
      "save game";
    parse_reset_inv_arg_num_exc_test "parse reset inv arg num exc test"
      game_state "reset game";
    parse_unknown_command_exc_test "parse unknown command exc test" game_state
      "purchase field 5";
  ]

let def =
  State.from_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "default_state.json"))

let save_tests =
  [ save_test "save test" (State.tick game_state); save_test "Reset" def ]

let suite =
  "test suite for Camel Project"
  >::: List.flatten
         [ camel_tests; state_tests; inputprocessor_tests; save_tests ]

let _ = run_test_tt_main suite
