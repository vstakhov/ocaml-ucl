(* open Core.Std *)
open Lexer
open Lexing
open Bi_outbuf

let print_position outx lexbuf =
	let pos = lexbuf.lex_curr_p in
	Printf.fprintf outx "%s:%d:%d" pos.pos_fname
		pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
	try Parser.ucl Lexer.read lexbuf with
	| SyntaxError msg ->
			Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
			None
	| Parser.Error ->
			Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
			exit (-1)
			
let rec output_value ob = function
	| `Assoc obj -> print_assoc ob obj
	| `List l -> print_list ob l
	| `String s -> Bi_outbuf.add_string ob (Printf.sprintf "\"%s\"" s)
	| `Int i -> Bi_outbuf.add_string ob (Printf.sprintf "%d" i)
	| `Float x -> Bi_outbuf.add_string ob (Printf.sprintf "%f" x)
	| `Bool true -> Bi_outbuf.add_string ob "true"
	| `Bool false -> Bi_outbuf.add_string ob "false"
	| `Null -> Bi_outbuf.add_string ob "null"

and print_assoc ob obj =
	Bi_outbuf.add_char ob '{';
	let sep = ref "" in
	List.iter ~f: (fun (key, value) ->
					Bi_outbuf.add_string ob (Printf.sprintf "%s\"%s\": %a" !sep key output_value(ob value));
					sep := ",\n ") obj;
	Bi_outbuf.add_char ob '}'

and print_list ob arr =
	Bi_outbuf.add_char ob '[';
	List.iteri ~f: (fun i v ->
					if i > 0 then
						Bi_outbuf.add_string ob ", ";
					output_value ob v) arr;
	Bi_outbuf.add_char ob ']'

let rec parse_to_ob_rec ob lexbuf =
	match parse_with_error lexbuf with
	| Some value ->
			output_value ob value;
			parse_to_ob ob lexbuf
	| None -> ob

let rec parse_to_ob lexbuf =
	parse_to_ob_rec Bi_outbuf.create lexbuf


let parse_file filename () =
	let inx = In_channel.create filename in
	let lexbuf = Lexing.from_channel inx in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
	parse_to_ob lexbuf;
	In_channel.close inx
	