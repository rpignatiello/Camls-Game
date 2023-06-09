type t

exception UnknownBuilding of string
exception NotEnoughMoney of string
exception UnknownResource of string

val from_json : Yojson.Basic.t -> t
(** [from_json] returns the game state loaded from player's save data *)

val quantity_of_building : t -> string -> int
(** [quantity_of_building] returns the [int] quantity of buildings of type
    [string] in game state [t] *)

val quantity_of_camel : t -> int
(** [quantity_of_camel] returns the [int] quantity of camels in game state [t] *)

val game_settings : t -> Camel.t
(** [game_settings] returns the game settings for game state [t]*)

val building_list : t -> string list
(** [building_list] returns the [string list] representation of buildings owned
    in the game state [t] *)

val get_resource : t -> string -> float
(** [get_resource] returns the [float] quantity of resource [string] in game
    state [t]*)

val edit_resource : t -> string -> float -> t
(** [edit_resource] returns a new game state [t] with the quantity of resource
    [string] increased by amount [float] *)

val trade : t -> string -> int -> t
(** [trade] exchangees the corresponding cost of purchasing [int] quantity of
    [string] resource and returns the updated state *)

val buy_building : t -> int -> string -> t
(** [buy_building] returns a new game state [t] with building [string] quantity
    increased by [int] if the player has enough resources to purchase *)

val tick : t -> t
(** [tick] returns the updated game state [t] after one unit of time has passed.
    Resource values are updated based on owned building production*)

val save : t -> unit
(** [save t] saves the state [t] onto state.json file. **)

val resource_list : t -> string list
(** [resource_list t] returns the list of resources in the game state [t] *)

val get_season : t -> string
(** [get_season t] returns the current season in the game state [t] *)

val get_day : t -> int
(** [get_day t] returns the day in the game state [t] *)

val add_camelnip : t -> t
(** [add_camelnip] increases and updates the amount of camelnip in the game
    state [t] *)

val cost : t -> string -> float
(** [cost t resource] returns the cost of a resource in the game state [t] *)

val calculate_camels : t -> int
(** [calculate_camels t] returns the total amount of camels in the game state
    [t] *)

val convert_buil_list : t -> string
(** [convert_buil_list t] returns the string version of the game state [t] *)
