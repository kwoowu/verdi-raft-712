type t = private string

val max_length_int : int
val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> t option
val unsafe_of_string : string -> t