type t

exception UnknownBuilding of string
exception NotEnoughMoney of string

val from_json : Yojson.Basic.t -> t
val quantity_of_building : t -> string -> int
val quantity_of_camel : t -> int
val building_list : t -> string list
val money : t -> float
val edit_money : t -> float -> t
val buy_building : t -> int -> string -> t
val tick : t -> t
