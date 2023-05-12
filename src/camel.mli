type t
type setting

exception UnknownBuilding of string
exception UnknownResource of string

val from_json : Yojson.Basic.t -> t
val item_for_building : t -> string -> string
val number_for_building : t -> string -> float
val produce_item_building : t -> string -> string
val production_rate_building : t -> string -> float
val contains_building : t -> string -> bool
val contains_resource : t -> string -> bool
val resource_settings : t -> string -> setting
val resource_cost : setting -> float
val resource_cost_type : setting -> string
