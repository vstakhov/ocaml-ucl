open Ucl_parser
open Ucl_emitter
open Core.Std

let loop filename () =
	try printf "%s\n" (Ucl_emitter.to_json (Ucl_parser.parse_file filename))
	with 
	| UCL_Syntax_Error (msg, state) -> 
		printf "Syntax error %s on line %d and column %d\n" msg state.line state.column;
		printf "%s\n" (Ucl_emitter.to_json state.top)
	| UCL_InternalError msg ->
		printf "Internal error %s" msg

let () =
	Command.basic ~summary:"Parse and display UCL"
		Command.Spec.(empty +> anon ("filename" %: file))
		loop
	|> Command.run