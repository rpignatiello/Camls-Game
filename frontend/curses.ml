open Curses
open Yojson.Basic

let rec main stdscr =
  let i = ref 0 in
  while true do
    (* Clear anything previously displayed on screen *)
    clear ();

    let f = open_in "./data/state.json" in
    let d = from_channel f in

    (* Printing json values and time that shows updating capability *)
    addstr ("Camels: " ^ (d |> member "Camels" |> to_string) ^ "\n");
    List.iter
      (fun b ->
        addstr
          (b |> member "name"
          |> to_string ^ ": " ^ (b |> member "quantity" |> to_string) ^ "\n"))
      (d |> member "Buildings" |> to_list);
    addstr ("time: " ^ string_of_int !i);
    i := !i + 1;

    refresh ();
    close_in f;
    Unix.sleepf 0.5
  done

let () = wrapper main
