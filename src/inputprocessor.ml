open Camel
open State

exception NoInput of string
exception InvalidInput of string

let buy cmd_list (state : State.t) =
  if List.length cmd_list <> 3 then
    raise
      (InvalidInput
         "Error: Incorrect Number of Arguments Provided. To buy a building use \
          the form [buy] [building] [quantity]")
  else if
    not (contains_building (State.game_settings state) (List.nth cmd_list 1))
  then
    raise
      (InvalidInput
         "Error: Building Not Found. Please Check that the Requested Building \
          Exists")
  else
    let quantity =
      try int_of_string (List.nth cmd_list 2)
      with Failure _ ->
        raise (InvalidInput "Invalid Quantity to Purchase Entered.")
    in
    if quantity <= 0 then
      raise (InvalidInput "Error: Quantity to Purchase Must be Greater than 0.")
    else buy_building state quantity (List.nth cmd_list 1)

let bonfire cmd_list (state : State.t) =
  if List.length cmd_list <> 1 || List.nth cmd_list 0 <> "gather" then
    raise
      (InvalidInput
         "Error: Incorrect Number of Arguments Provided. To gather camelnip, \
          type [gather]")
  else add_camelnip state

let trade cmd_list (state : State.t) =
  if List.length cmd_list <> 3 then
    raise
      (InvalidInput
         "Error: Incorrect Number of Arguments Provided. To trade resources \
          use the form [trade] [target resource] [quantity]")
  else if
    not (contains_resource (State.game_settings state) (List.nth cmd_list 1))
  then
    raise
      (InvalidInput
         "Error: Resource Not Found. Please Check that the Requested Resource \
          Exists")
  else
    let quantity =
      try int_of_string (List.nth cmd_list 2)
      with Failure _ ->
        raise (InvalidInput "Invalid Quantity to Trade for Entered")
    in
    if quantity <= 0 then
      raise
        (InvalidInput "Error: Quantity to Trade for Must be Greater than 0.")
    else trade state (List.nth cmd_list 1) quantity

let save cmd_list (state : State.t) =
  if List.length cmd_list <> 1 then
    raise (InvalidInput "To save game data, type [save]")
  else save state;
  state

let data_dir_prefix = "data" ^ Filename.dir_sep

let def =
  State.from_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "default_state.json"))

let reset cmd_list =
  if List.length cmd_list <> 1 then
    raise (InvalidInput "To reset save data, type [reset]")
  else State.save def;
  def

let string_empty s = if s = " " || s = "" then false else true

let rec parse_input input state =
  let cmd_list =
    List.filter string_empty
      (String.split_on_char ' ' (String.lowercase_ascii input))
  in
  match cmd_list with
  | [] -> raise (NoInput "Error: Please Input a Command")
  | "buy" :: _ -> buy cmd_list state
  | "trade" :: _ -> trade cmd_list state
  | "gather" :: _ -> bonfire cmd_list state
  | "g" :: _ -> bonfire [ "gather" ] state
  | "save" :: _ -> save cmd_list state
  | "reset" :: _ -> reset cmd_list
  | "pause" :: _ ->
      Printf.printf "\n Game is paused. Press enter to continue...";
      flush stdout;
      ignore (Unix.read Unix.stdin (Bytes.create 1) 0 1);
      state
  | "help" :: _ ->
      Printf.printf "\n  Available commands: ";
      Printf.printf "\n1) [buy][name of resource ex. field][quantity ex. 1]";
      Printf.printf "\n 2) [trade][name of resource ex. field][quantity ex. 1]";
      Printf.printf "\n 3) [gather]";
      Printf.printf "\n 4) [save]";
      Printf.printf "\n 5) [reset]";
      Printf.printf "\n 6) [help]";
      Printf.printf "\n 7) [pause]";
      Printf.printf "\n\n Press enter to continue...";
      flush stdout;
      ignore (Unix.read Unix.stdin (Bytes.create 1) 0 1);
      state
  | _ -> raise (InvalidInput "Error: Invalid Input")
