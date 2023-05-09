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

let string_empty s = if s = " " || s = "" then false else true

let parse_input input state =
  let cmd_list =
    List.filter string_empty
      (String.split_on_char ' ' (String.lowercase_ascii input))
  in
  match cmd_list with
  | [] -> raise (NoInput "Error: Please Input a Command")
  | "buy" :: t when t <> [] -> buy cmd_list state
  | _ -> raise (InvalidInput "Error: Invalid Input")
