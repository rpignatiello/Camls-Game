exception NoInput of string
exception InvalidInput of string

val buy : string list -> State.t -> State.t
(** [buy] attempts to return the updated game state [t] after a building
    purchase is made. Checks the preconditions that the input is legal *)

val parse_input : string -> State.t -> State.t
(** Parses the user input [string] and returns game state [t] after updating it
    with the input's corresponding commands *)
