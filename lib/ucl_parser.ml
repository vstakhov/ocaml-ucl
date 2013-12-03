open Ucl
open Ucl_util
open Buffer
open Stream

type ucl_state =
	| UCL_STATE_INIT
	| UCL_STATE_READ_KEY
	| UCL_STATE_READ_VALUE
	| UCL_STATE_AFTER_VALUE
	| UCL_STATE_COMMENT

type ucl_parser_state = {
	line : int;
	column : int;
	state : ucl_state;
	prev_state : ucl_state;
	buf : Buffer.t;
	stack : ucl list;
	top : ucl;
}

let u_nl = 0x0A (* \n *)
let u_sp = 0x20 (* *)
let u_quot = 0x22 (* '' *)
let u_lbrack = 0x5B (* [ *)
let u_rbrack = 0x5D (* ] *)
let u_lbrace = 0x7B (* { *)
let u_rbrace = 0x7D (* } *)
let u_colon = 0x3A (* : *)
let u_dot = 0x2E (* . *)
let u_comma = 0x2C (* , *)
let u_minus = 0x2D (* - *)
let u_slash = 0x2F (* / *)
let u_bslash = 0x5C (* \ *)
let u_times = 0x2A (* * *)

let is_white_safe c =
	match Char.code c with
	| 0x20 | 0x09 | 0x0A -> true
	| _ -> false

let parser_handle_init state line =
	match line.[state.column] with
	| '#' -> { state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT}
	| '{' -> let ntop = `Assoc (Hashtbl.create 32) in 
		{
			state with 
			top = ntop; 
			stack = ntop :: state.stack; column = state.column + 1; 
			state = UCL_STATE_READ_KEY
		}
	| '[' -> let ntop = `List [] in 
		{ 
			state with 
			top = ntop; 
			stack = ntop :: state.stack; 
			column = state.column + 1;
			state = UCL_STATE_READ_VALUE
		}
	| c -> if is_white_safe c then 
		{state with column = state.column + 1 }
		else (* Assume object *)
		let  ntop = `Assoc (Hashtbl.create 32) in 
		{
			state with 
			top = ntop; 
			stack = ntop :: state.stack;
			state = UCL_STATE_READ_KEY
		}

let parser_handle_key state line = 
	state
	
let parser_handle_value state line =
	state
	
let parser_handle_after_value state line = 
	state
	
let parser_handle_comment state line = 
	state

let rec parser_state_machine state inx =
	let parser_parse_line state line = 
		match state.state with
		| UCL_STATE_INIT ->
			parser_state_machine (parser_handle_init state line) inx
		| UCL_STATE_READ_KEY ->
			parser_state_machine (parser_handle_key state line) inx
		| UCL_STATE_READ_VALUE ->
			parser_state_machine (parser_handle_value state line) inx
		| UCL_STATE_AFTER_VALUE ->
			parser_state_machine (parser_handle_after_value state line) inx
		| UCL_STATE_COMMENT ->
			parser_state_machine (parser_handle_comment state line) inx
	in
	try
		let line = input_line inx in
		parser_parse_line {state with line = state.line + 1} line
	with End_of_file -> state

let rec parse_in_channel inx =
	let init_state = {
		line = 0;
		column = 0;
		state = UCL_STATE_INIT;
		buf = Buffer.create 32;
		stack = [];
		top = `Null;
		prev_state = UCL_STATE_INIT;
	} in
	let final_state = parser_state_machine init_state inx in
	final_state.top

let parse_file filename =
	let inx = open_in filename in
	with_input_channel inx parse_in_channel