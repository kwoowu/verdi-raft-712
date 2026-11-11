(* stand-in for Rocq's kernel Pstring module. if we try to link Rocq's kernel library,
   everything breaks because there are conflicting names (I think "Util" specifically)
   between the Rocq kernel and the Verdi runtime :p *)
type t = string

let compare : t -> t -> int = String.compare
let equal : t -> t -> bool = String.equal

let max_length_int = 16777211

let to_string (s : t) : string = s
let unsafe_of_string (s : string) : t = s

let of_string (s : string) : t option =
  if String.length s <= max_length_int then Some s else None

