type t
type setting

exception UnknownBuilding of string
exception UnknownResource of string
exception UnknownSeason of string

val from_json : Yojson.Basic.t -> t
(** [from_json] returns the game state loaded from player's save data *)

val item_for_building : t -> string -> string
(** [item_for_building] returns the [string] item of buildings of type [string]
    in game state [t] *)

val number_for_building : t -> string -> float
(** [numer_for_building] returns the [float] amount needed to build a building
    of type [string] in game state [t] *)

val produce_item_building : t -> string -> string
(** [produce_item_building] returns the [string] item produced from a building
    of type [string] in game state [t] *)

val production_rate_building : t -> string -> float
(** [production_rate_building] returns the [float] production rate for a
    building of type [string] in game state [t] *)

val contains_building : t -> string -> bool
(** [contains_building] returns the [bool] result if a building of name [string]
    is contained in the game state [t] *)

val contains_resource : t -> string -> bool
(** [contains_resource user r] returns [true] if the [user] does have resource
    [r] **)

val resource_settings : t -> string -> setting
(** [resource_settings user r] returns what is required to make resource r and
    the amount of that material *)

val season_multiplier : t -> string -> float
(** [season_multiplier user s] returns the rate as a float at which camelnip
    increases depending on the season [s] *)

val next_season : t -> string -> string
(** [next_season user s] returns the next season after the current season [s]*)

val resource_cost : setting -> float
(** [resource_cost setting] returns the cost as a float of making a resource *)

val resource_cost_type : setting -> string
(** [resource_cost_type] returns the resource used to make that resource in
    settings *)

val cost_multiplier : t -> string -> float
(** [cost_multiplier user building] returns the cost as a float of making a
    building *)
