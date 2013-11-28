open Ucl_parser
open Core.Std

let loop filename () =
	printf "%s\n" (Ucl_parser.parse_file filename)

let () =
	Command.basic ~summary:"Parse and display UCL"
		Command.Spec.(empty +> anon ("filename" %: file))
		loop
	|> Command.run