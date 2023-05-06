type t

exception UnknownBuilding of string

val from_json : Yojson.Basic.t -> t
val quantity_of_building : t -> string -> int
val quantity_of_camel : t -> int
val building_list : t -> string list
