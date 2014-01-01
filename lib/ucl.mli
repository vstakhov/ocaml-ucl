open Hashtbl

type ucl = [
	| `Assoc of (string, ucl list) Hashtbl.t
	| `Bool of bool
	| `Float of float
	| `Int of int
	| `List of ucl list
	| `Null
	| `String of string
]