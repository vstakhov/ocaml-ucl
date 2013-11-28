open Lexing
open Lexer
open Ucl

let print_position outx lexbuf =
	let pos = lexbuf.lex_curr_p in
	Printf.fprintf outx "%s:%d:%d" pos.pos_fname
		pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
	try Parser.parse Lexer.read lexbuf with
	| SyntaxError msg ->
			Printf.fprintf stderr "%a: lexer error: %s\n" print_position lexbuf msg;
			None
	| Parser.Error ->
			Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
			exit (-1)
			
let rec output_obj init obj = 
	match obj with
	| `Assoc obj -> output_assoc init obj
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
			if buf == "" then
				buf ^ (output_obj "" v)
			else 
				buf ^ ", " ^ (output_obj "" v)
			) input arr
	in
	init ^ "[" ^ list_out "" ^ "]"

and output_assoc init obj =
	let obj_out input =
		List.fold_left (fun buf (key, value) ->
				buf ^ (Printf.sprintf "%s\"%s\": %s" 
						(if buf == "" then "" else ",\n") 
						key (output_obj "" value))
				) input obj
	in
	init ^ "{" ^ obj_out "" ^ "}"

let rec parse_to_string lexbuf =
	let rec parse_to_string_rec str =
		match parse_with_error lexbuf with
		| Some value ->
				parse_to_string_rec (output_obj str value)
		| None -> str
	in
	parse_to_string_rec ""

(* Try ... Finally emulation *)
let unwind ~protect f x =
  let module E = struct type 'a t = Left of 'a | Right of exn end in
  let res = try E.Left (f x) with e -> E.Right e in
  let ()  = protect x in
  match res with
  | E.Left  y -> y
  | E.Right e -> raise e

let with_input_channel inch f =
  unwind ~protect:close_in f inch

let parse_file filename =
	let parse_in_channel inx =
		let lexbuf = Lexing.from_channel inx in
		lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
		parse_to_string lexbuf
	in
	let inx = open_in filename in
	with_input_channel inx parse_in_channel