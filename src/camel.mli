type t

exception UnknownBuilding of string

val from_json : Yojson.Basic.t -> t
val item_for_building : t -> string -> string
