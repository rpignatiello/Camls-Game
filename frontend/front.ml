open Curses
open Yojson.Basic.Util
open CamlGame
open Inputprocessor

type error_message = {
  message : string;
  time : int;
}

(* let rec main stdscr = let i = ref 0 in while true do (* Clear anything
   previously displayed on screen *) clear (); let data_dir_prefix = "data" ^
   Filename.dir_sep in let d = Yojson.Basic.from_file (data_dir_prefix ^
   "state.json") in let c = State.from_json d in

   (* Printing json values and time that shows updating capability *) if not
   (addstr ("Camels: " ^ string_of_int (State.quantity_of_camel c) ^ "\n")) then
   failwith "Error" else (); List.iter (fun b -> if not (addstr (b ^ ": " ^
   string_of_int (State.quantity_of_building c b) ^ "\n")) then failwith
   "Error!" else ()) (State.building_list c); if not (addstr ("time: " ^
   string_of_int !i)) then failwith "Error" else (); i := !i + 1;

   if not (refresh ()) then failwith "error" else (); Unix.sleepf 0.5 done *)

let () =
  let main_window = initscr () in
  let _ = nodelay main_window true in
  let _ = noecho in
  let i = ref 0 in
  let ch = ref "" in
  let err = ref { message = ""; time = 0 } in
  let data_dir_prefix = "data" ^ Filename.dir_sep in
  let d = Yojson.Basic.from_file (data_dir_prefix ^ "state.json") in
  let c = ref (State.from_json d) in
  while true do
    (* Clear anything previously displayed on screen *)
    ignore (clear ());

    (* Printing json values and time that shows updating capability *)
    ignore
      (waddstr main_window
         ("Camels: " ^ string_of_int (State.quantity_of_camel !c) ^ "\n"));
    (* then failwith "Error" else (); *)
    List.iter
      (fun b ->
        if
          not
            (waddstr main_window
               (b ^ ": "
               ^ string_of_int (State.quantity_of_building !c b)
               ^ "\n"))
        then failwith "Error!"
        else ())
      (State.building_list !c);
    List.iter
      (fun b ->
        if
          not
            (waddstr main_window
               (b ^ ": " ^ string_of_float (State.get_resource !c b) ^ "\n"))
        then failwith "Error!"
        else ())
      (State.resource_list !c);

    if not (waddstr main_window ("time: " ^ string_of_int (!i / 10))) then
      failwith "Error"
    else ();
    let input = getch () in
    let tmp = if input = -1 then "" else String.make 1 (char_of_int input) in
    if tmp = ";" then (
      try
        let _ = c := Inputprocessor.parse_input !ch !c in
        ch := ""
      with error ->
        let r = Printexc.to_string error in
        ch := "";
        err := { message = r; time = 100 })
    else ch := !ch ^ tmp;
    let _ = waddstr main_window ("\n" ^ "User input: " ^ !ch ^ "\n") in
    let _ =
      if !err.time > 0 then
        let _ = waddstr main_window ("\n" ^ !err.message ^ "\n") in
        err := { message = !err.message; time = !err.time - 1 }
      else err := { message = !err.message; time = !err.time - 1 }
    in
    i := !i + 1;

    if not (refresh ()) then failwith "error" else ();
    if !i mod 10 = 0 then c := State.tick !c else c := !c;
    ignore (Unix.select [] [] [] 0.05)
  done;
  endwin ()
