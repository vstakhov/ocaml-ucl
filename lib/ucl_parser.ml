open Ucl
open Ucl_util

let parse_in_channel inx =
	None

let parse_file filename =
	let inx = open_in filename in
	with_input_channel inx parse_in_channel