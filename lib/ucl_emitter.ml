open Ucl

let rec json_output_obj init obj = 
	match obj with
	| `Assoc a -> output_assoc init a
	| `List l -> output_list init l
	| `String s -> init ^ (Printf.sprintf "\"%s\"" s)
	| `Int i -> init ^ (Printf.sprintf "%d" i)
	| `Float x -> init ^ (Printf.sprintf "%f" x)
	| `Bool true -> init ^ "true"
	| `Bool false -> init ^ "false"
	| `Null -> init ^ "null"

and output_list init arr =
	let list_out input =
		List.fold_left (fun buf v ->
			if buf = "" then
				buf ^ (json_output_obj "" v)
			else 
				buf ^ ", " ^ (json_output_obj "" v)
			) input arr
	in
	init ^ "[\n" ^ list_out "" ^ "\n]"

and output_assoc init obj =
	let obj_out input =
		Hashtbl.fold (fun key value buf ->
				buf ^ (Printf.sprintf "%s\"%s\": %s" 
						(if buf = "" then "" else ",\n") 
						key (output_list "" value))
				) obj init
	in
	init ^ "{\n" ^ obj_out "" ^ "\n}"
	
let to_json obj =
	json_output_obj "" obj
	