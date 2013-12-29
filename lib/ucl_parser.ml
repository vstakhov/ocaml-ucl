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
	| UCL_STATE_END

type ucl_buf = [
	| `Buf of Buffer.t
	| `Null
]

type ucl_parser_state = {
	line : int;
	column : int;
	pos : int;
	state : ucl_state;
	prev_state : ucl_state;
	buf : ucl_buf;
	key : ucl_buf;
	value : ucl_buf;
	stack : ucl list;
	top : ucl;
	remain : int;
}

exception UCL_Syntax_Error of (string * ucl_parser_state)
exception UCL_InternalError of string

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

(** Match any whitespace character including newlines *)
let is_white_unsafe c =
	match Char.code c with
	| c when c = u_sp || c = u_nl || c = u_tab -> true
	| _ -> false

(** Match whitespace character excluding newlines *)
let is_white_safe c =
	match Char.code c with
	| c when c = u_sp || c = u_tab -> true
	| _ -> false

(** Go to the next char *)
let parser_next_char state line =
	if state.remain > 0 then
		match line.[state.column] with
		| '\n' -> { state with 
									column = 0; 
									line = state.line + 1; 
									pos = state.pos + 1; 
									remain = state.remain - 1}
		| c -> { state with 
									column = state.column + 1; 
									pos = state.pos + 1; 
									remain = state.remain - 1}
	else
		{ state with state = UCL_STATE_END }

(** Append new object or array to the stack of parser *)
let parser_new_object state line ?(skip_char=true) ?(is_array=false) ?(set_top=false) () =
	let nobj = if is_array then `List [] else `Assoc (Hashtbl.create 32) in
	if skip_char then
		{
		(parser_next_char state line) with
		state = if is_array then UCL_STATE_READ_VALUE else UCL_STATE_READ_KEY;
		stack = nobj :: state.stack; 
		top = if set_top then nobj else state.top;
		}
	else
		{
		state with
		state = if is_array then UCL_STATE_READ_VALUE else UCL_STATE_READ_KEY;
		stack = nobj :: state.stack; 
		top = if set_top then nobj else state.top;
		}

let parser_buf_add_char buf c =
	match buf with
	| `Buf b -> Buffer.add_char b c
	| `Null -> raise (UCL_InternalError "Buffer is not initialized")

(** Check for multiline comment if previous char is '/' *)
let parser_is_comment state line =
	if state.remain > 0 then
		if (state.pos > 1 && line.[state.pos] = '/' && line.[state.pos + 1] = '*') ||
		 line.[state.pos] = '#' then
			true
		else
			false
	else
		false

(** Skip characters when predicate is true *)	
let rec parser_skip_chars test_func state line =
	if state.remain > 0 then
		if test_func line.[state.pos] then
			parser_skip_chars test_func (parser_next_char state line) line
		else
			state
	else
		state

(** Parse a quoted string and decode JSON escapes *)
let rec parser_read_quoted_string state line = 
	match line.[state.pos] with
	| '\\' -> parser_read_quoted_string 
			(parser_read_escape_sequence (parser_next_char state line) line) line
	| '"' -> state
	| c -> parser_buf_add_char state.buf c; 
		parser_read_quoted_string (parser_next_char state line) line
and parser_read_escape_sequence state line = 
	match line.[state.pos] with
	| 'n' -> parser_buf_add_char state.buf '\n'; state
	| 'r' -> parser_buf_add_char state.buf '\r'; state
	| 't' -> parser_buf_add_char state.buf '\t'; state
	| 'b' -> parser_buf_add_char state.buf '\b'; state
	| '\\' -> parser_buf_add_char state.buf '\\'; state
	| 'u' -> parser_read_unicode_escape (parser_next_char state line) line
	| _ -> raise (UCL_Syntax_Error ("Invalid escape character", state))
and parser_read_unicode_escape state line =
	let rec parser_unicode_helper res level state line =
		if level == 3 then
			(parser_add_unicode_character res state; state)
		else
			match line.[state.pos] with
			| '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
				let newres = (res lsl 4) + (parser_hexcode line.[state.pos] state) in
				parser_unicode_helper newres (level + 1) (parser_next_char state line) line
			| _ -> raise (UCL_Syntax_Error ("Invalid unicode escape character", state))
	in
	parser_unicode_helper 0 0 state line
and parser_hexcode c state =
	let lc = Char.lowercase c in
	match lc with
	| '0' .. '9' -> Char.code lc - Char.code '0'
	| 'a' .. 'f' -> Char.code lc - Char.code 'a' + 10
	| c -> raise (UCL_Syntax_Error ("Invalid hex character", state))
and parser_add_unicode_character num state =
	if num < 0x80 then
		parser_buf_add_char state.buf (char_of_int num)
	else if num < 0x800 then
		(
			parser_buf_add_char state.buf (char_of_int (0xC0 + ((num land 0x7C0) lsr 6)));
			parser_buf_add_char state.buf (char_of_int (0x80 + (num land 0x3F)))
		)
	else if num < 0x10000 then
		(
			parser_buf_add_char state.buf (char_of_int (0xE0 + ((num land 0xF000) lsr 12)));
			parser_buf_add_char state.buf (char_of_int (0x80 + ((num land 0xFC0) lsr 6)));
			parser_buf_add_char state.buf (char_of_int (0x80 + (num land 0x3F)))
		)
	else if num <= 0x10FFFF then
		(
			parser_buf_add_char state.buf (char_of_int (0xF0 + ((num land 0x1C0000) lsr 18)));
			parser_buf_add_char state.buf (char_of_int (0x80 + ((num land 0x03F000) lsr 12)));
			parser_buf_add_char state.buf (char_of_int (0x80 + ((num land 0xFC0) lsr 6)));
			parser_buf_add_char state.buf (char_of_int (0x80 + (num land 0x3F)))
		)
	else
		raise (UCL_Syntax_Error ("Invalid unicode escape value", state))

(** Parser init state handler *)
let parser_handle_init state line =
	match line.[state.pos] with
	| '#' -> { state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT }
	| '{' -> parser_new_object state line ~set_top:true ()
	| '[' -> parser_new_object state line ~is_array:true ~set_top:false ()
	| '/' -> 
		if parser_is_comment state line then
			{ state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT }
		else 
			raise (UCL_Syntax_Error ("Invalid starting character", state))
	| c -> 
		if is_white_unsafe c then 
			parser_skip_chars is_white_unsafe state line
		else (* Assume object *)
			parser_new_object state line ~skip_char:false ()

(** Parser key state handler *)
let rec parser_handle_key state line = 
	match line.[state.column] with
	| '/' | '#' -> 
		if parser_is_comment state line then
			{ state with prev_state = UCL_STATE_READ_KEY; state = UCL_STATE_COMMENT }
		else 
			raise (UCL_Syntax_Error ("Invalid starting character", state))
	| c ->
		if is_white_safe c then
			(* Skip whitespaces at the beginning *)
			parser_handle_key (parser_skip_chars is_white_safe state line) line
		else
			match c with
			| '"' -> 
				{
					(parser_read_quoted_string {(parser_next_char state line) with buf = `Buf(Buffer.create 32) } line) with
					prev_state = UCL_STATE_READ_KEY; 
					state = UCL_STATE_READ_VALUE;
					key = state.buf
				}
			| c -> state
		
	
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
			| UCL_STATE_END -> state
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
		pos = 0;
		state = UCL_STATE_INIT;
		buf = `Null;
		key = `Null;
		value = `Null;
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