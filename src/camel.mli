type t

exception UnknownBuilding of string

val from_json : Yojson.Basic.t -> t
val item_for_building : t -> string -> string
val number_for_building : t -> string -> int
val produce_item_building : t -> string -> string
val production_rate_building : t -> string -> float
