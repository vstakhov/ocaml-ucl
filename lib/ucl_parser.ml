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
	remain : int;
}

exception UCL_Syntax_Error of (string * ucl_parser_state)

let u_nl = 0x0A (* \n *)
let u_sp = 0x20 (* *)
let u_tab = 0x09 (* *)
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

let is_white_unsafe c =
	match Char.code c with
	| c when c = u_sp || c = u_nl || c = u_tab -> true
	| _ -> false

let parser_new_object state ?(skip_char=true) ?(is_array=false) ?(set_top=false) () =
	let nobj = if is_array then `List [] else `Assoc (Hashtbl.create 32) in
	{
		state with
		column = if skip_char then state.column + 1 else state.column;
		remain = if skip_char then state.remain - 1 else state.remain;
		state = if is_array then UCL_STATE_READ_VALUE else UCL_STATE_READ_KEY;
		stack = nobj :: state.stack; 
		top = if set_top then nobj else state.top;
	}

let parser_is_comment state line =
	if state.remain > 0 && line.[state.column + 1] = '*' then
		true
	else
		false

let parser_handle_init state line =
	match line.[state.column] with
	| '#' -> { state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT }
	| '{' -> parser_new_object state ~set_top:true ()
	| '[' -> parser_new_object state ~is_array:true ~set_top:false ()
	| '/' -> 
		if parser_is_comment state line then
			{ state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT }
		else 
			raise (UCL_Syntax_Error ("Invalid starting character", state))
			
	| c -> 
		if is_white_unsafe c then 
			{ state with column = state.column + 1 }
		else (* Assume object *)
			parser_new_object state ~skip_char:false ()

let parser_handle_key state line = 
	state
	
let parser_handle_value state line =
	state
	
let parser_handle_after_value state line = 
	state
	
let parser_handle_comment state line = 
	state

let parser_parse_stream state inx =
	let rec parser_state_machine state init inx =
		let parser_parse_line state line =
			match state.state with
			| UCL_STATE_INIT ->
					parser_state_machine (parser_handle_init state line) line inx
			| UCL_STATE_READ_KEY ->
					parser_state_machine (parser_handle_key state line) line inx
			| UCL_STATE_READ_VALUE ->
					parser_state_machine (parser_handle_value state line) line inx
			| UCL_STATE_AFTER_VALUE ->
					parser_state_machine (parser_handle_after_value state line) line inx
			| UCL_STATE_COMMENT ->
					parser_state_machine (parser_handle_comment state line) line inx
		in
		if state.remain > 0 then
			parser_parse_line state init
		else
			try
				let line = input_line inx in
				parser_parse_line { state with
						line = state.line + 1;
						remain = String.length line; } line
			with End_of_file -> state
	in
	parser_state_machine state "" inx

let parse_in_channel inx =
	let init_state = {
		line = 0;
		column = 0;
		state = UCL_STATE_INIT;
		buf = Buffer.create 32;
		stack = [];
		top = `Null;
		prev_state = UCL_STATE_INIT;
		remain = 0;
	} in
	let final_state = parser_parse_stream init_state inx in
	final_state.top

let parse_file filename =
	let inx = open_in filename in
	with_input_channel inx parse_in_channel