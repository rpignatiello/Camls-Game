type t

exception UnknownBuilding of string
exception NotEnoughMoney of string

val from_json : Yojson.Basic.t -> t
val quantity_of_building : t -> string -> int
val quantity_of_camel : t -> int
val building_list : t -> string list
val get_resource : t -> string -> float
val edit_resource : t -> string -> float -> t
val buy_building : t -> int -> string -> t
val tick : t -> t
