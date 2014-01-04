open Ucl
open Ucl_util
open Buffer
open Stream

type ucl_state =
	| UCL_STATE_INIT
	| UCL_STATE_READ_KEY
	| UCL_STATE_AFTER_KEY
	| UCL_STATE_READ_VALUE
	| UCL_STATE_AFTER_VALUE_UNQUOTED
	| UCL_STATE_AFTER_VALUE_QUOTED
	| UCL_STATE_COMMENT_START
	| UCL_STATE_COMMENT_SINGLE
	| UCL_STATE_COMMENT_MULTI
	| UCL_STATE_END

type ucl_parser_state = {
	line : int;
	column : int;
	pos : int;
	state : ucl_state;
	prev_state : ucl_state;
	buf : Buffer.t option;
	key : Buffer.t option;
	value : Buffer.t option;
	stack : ucl list;
	top : ucl;
	remain : int;
	comment_level : int;
}

exception UCL_Syntax_Error of (string * ucl_parser_state)
exception UCL_InternalError of string

(** Debug output of the current state *)
let parser_print_state state line =
	let state_to_str state =
		match state with
		| UCL_STATE_INIT -> "init"
		| UCL_STATE_READ_KEY -> "read_key"
		| UCL_STATE_AFTER_KEY -> "after_key"
		| UCL_STATE_READ_VALUE -> "read_value"
		| UCL_STATE_AFTER_VALUE_UNQUOTED -> "after_value(unquoted)"
		| UCL_STATE_AFTER_VALUE_QUOTED -> "after_value(quoted)"
		| UCL_STATE_COMMENT_START -> "start_comment"
		| UCL_STATE_COMMENT_SINGLE -> "single_comment"
		| UCL_STATE_COMMENT_MULTI -> "multi_comment"
		| UCL_STATE_END -> "end"
	in
	Printf.printf "cur_state: %s, prev_state: %s, objects in stack: %d\n"
		(state_to_str state.state) (state_to_str state.prev_state) (List.length state.stack);
	if state.remain > 0 then
		Printf.printf "line: %d, column: %d, pos: %d, remain: %d, char: %c\n"
			state.line state.column state.pos state.remain line.[state.pos]
	else
		Printf.printf "line: %d, column: %d, pos: %d, remain: %d\n"
			state.line state.column state.pos state.remain

(** Switch parser state *)
let parser_switch_state state new_state =
	{ state with prev_state = state.state; state = new_state;}

(** Go to the next char *)
let parser_next_char state line =
	if state.remain > 0 then
		match line.[state.pos] with
		| '\n' -> { state with
					column = 1;
					line = state.line + 1;
					pos = state.pos + 1;
					remain = state.remain - 1 }
		| c -> { state with
					column = state.column + 1;
					pos = state.pos + 1;
					remain = state.remain - 1 }
	else
		parser_switch_state state UCL_STATE_END

(** Append new object or array to the stack of parser *)
let parser_push_object_stack state line
		?(skip_char = true) ?(is_array = false) ?(set_top = false) () =
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

(** Pop an object from stack *)
let parser_pop_object_stack state line =
	match state.stack with
	| [] -> raise (UCL_InternalError "Invalid object in stack")
	| elt:: tail -> {
				(parser_next_char state line) with
				stack = tail;
			}

(** Add a resulting object *)
let parser_add_object state nobj =
	let top = List.hd state.stack in
	match top with
	| `Assoc a -> (match state.key with
				| Some v -> let key = Buffer.contents v in
						let found = try Hashtbl.find a key with
							| Not_found -> []
						in
						Hashtbl.replace a key (nobj :: found);
						{ state with
							key = None;
							value = None;
						}
				| None -> raise (UCL_InternalError "No key defined"))
	| `List l -> { state with
				key = None;
				value = None;
				stack = nobj :: l @ (List.tl state.stack) (* TODO: slow *)
			}
	| _ -> raise (UCL_InternalError "Invalid object in stack")

let parser_buf_add_char buf c =
	match buf with
	| Some b -> Buffer.add_char b c
	| None -> raise (UCL_InternalError "Buffer is not initialized")

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
	if state.remain == 0 then
		raise (UCL_Syntax_Error ("Unfinished quoted string", state))
	else
		(
			match line.[state.pos] with
			| '\\' -> parser_read_quoted_string
						(parser_read_escape_sequence (parser_next_char state line) line) line
			| '"' -> parser_next_char state line
			| c -> parser_buf_add_char state.buf c;
					parser_read_quoted_string (parser_next_char state line) line
		)
and parser_read_escape_sequence state line =
	match line.[state.pos] with
	| 'n' -> parser_buf_add_char state.buf '\n'; parser_next_char state line
	| 'r' -> parser_buf_add_char state.buf '\r'; parser_next_char state line
	| 't' -> parser_buf_add_char state.buf '\t'; parser_next_char state line
	| 'b' -> parser_buf_add_char state.buf '\b'; parser_next_char state line
	| '\\' -> parser_buf_add_char state.buf '\\'; parser_next_char state line
	| '"' -> parser_buf_add_char state.buf '"'; parser_next_char state line
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

(** Read unquoted string and place it inside the state buffer *)
let rec parser_read_unquoted_string state line =
	if state.remain == 0 || isvalueend line.[state.pos] then
		state
	else (
		parser_buf_add_char state.buf line.[state.pos];
		parser_read_unquoted_string (parser_next_char state line) line
	)

(** Parser init state handler *)
let parser_handle_init state line =
	parser_print_state state line;
	match line.[state.pos] with
	| '#' -> { state with prev_state = UCL_STATE_INIT; state = UCL_STATE_COMMENT_START }
	| '{' -> parser_push_object_stack state line ~set_top: true ()
	| '[' -> parser_push_object_stack state line ~is_array: true ~set_top: false ()
	| '/' ->
			if parser_is_comment state line then
				parser_switch_state state UCL_STATE_COMMENT_START
			else
				raise (UCL_Syntax_Error ("Invalid starting character", state))
	| c ->
			if Ucl_util.isspace_unsafe c then
				parser_skip_chars Ucl_util.isspace_unsafe state line
			else (* Assume object *)
			parser_push_object_stack state line ~skip_char: false ()

(** Parser key state handler *)
let rec parser_handle_key state line =
	parser_print_state state line;
	match line.[state.pos] with
	| '/' | '#' ->
			if parser_is_comment state line then
				parser_switch_state state UCL_STATE_COMMENT_START
			else
				raise (UCL_Syntax_Error ("Invalid starting character", state))
	| '}' -> parser_switch_state state UCL_STATE_AFTER_VALUE_UNQUOTED
	| c ->
			if Ucl_util.isspace_safe c then
				(* Skip whitespaces at the beginning *)
				parser_handle_key (parser_skip_chars Ucl_util.isspace_safe state line) line
			else
				match c with
				| '"' ->
				(* JSON like string *)
						{
							(parser_read_quoted_string
									{ (parser_next_char state line) with buf = Some (Buffer.create 32) }
									line) with
							prev_state = UCL_STATE_READ_KEY;
							state = UCL_STATE_AFTER_KEY;
							key = state.buf
						}
				| c ->
						if Ucl_util.iskeystart c then
							(* Unquoted string *)
							{
								(parser_read_unquoted_string
										{ (parser_next_char state line) with buf = Some (Buffer.create 32) }
										line) with
								prev_state = UCL_STATE_READ_KEY;
								state = UCL_STATE_AFTER_KEY;
								key = state.buf
							}
						else
							raise (UCL_Syntax_Error ("Key begins with an invalid character", state))

(** Handle after key state *)
let parser_handle_after_key state line =
	parser_print_state state line;
	let rec parser_after_key_helper state line has_sep =
		let rec skip_spaces state line =
			let c = line.[state.pos] in
			if Ucl_util.isspace_safe c then
				skip_spaces (parser_next_char state line) line
			else
				state
		in
		match line.[state.pos] with
		| ':' | '=' ->
				parser_switch_state (skip_spaces (parser_next_char state line) line) UCL_STATE_READ_VALUE
		| ' ' | '\t' -> parser_after_key_helper (skip_spaces state line) line true
		| '{' | '[' -> parser_switch_state state UCL_STATE_READ_VALUE
		| c ->
				if has_sep then
					parser_switch_state state UCL_STATE_READ_VALUE
				else
					raise (UCL_Syntax_Error ("Invalid character at the end of the key", state))
	in
	if state.remain > 0 then parser_after_key_helper state line false
	else raise (UCL_Syntax_Error ("Key is unfinished", state))

(** Handle value parsing state *)
let rec parser_handle_value state line =
	parser_print_state state line;
	match line.[state.pos] with
	| '{' -> let nobj = `Assoc (Hashtbl.create 32) in
			parser_push_object_stack (parser_add_object state nobj) line ()
	| '[' -> let nobj = `List [] in
			parser_push_object_stack (parser_add_object state nobj) line ~is_array: true ()
	| '"' -> (* JSON like string *)
			{
				(parser_read_quoted_string
						{ (parser_next_char state line) with buf = Some (Buffer.create 32) }
						line) with
				prev_state = UCL_STATE_READ_VALUE;
				state = UCL_STATE_AFTER_VALUE_QUOTED;
				value = state.buf
			}
	| c ->
			if Ucl_util.isspace_safe c then
				parser_handle_value (parser_skip_chars Ucl_util.isspace_unsafe state line) line
			else
				{
					(parser_read_unquoted_string
							{ state with buf = Some (Buffer.create 32) }
							line) with
					prev_state = UCL_STATE_READ_VALUE;
					state = UCL_STATE_AFTER_VALUE_UNQUOTED;
					value = state.buf
				}

let parser_handle_after_value state line is_quoted =
	parser_print_state state line;
	let parser_finish_def state line =
		match state.stack with
		| [] -> parser_switch_state state UCL_STATE_END
		| _ ->
				(match List.hd state.stack with
					| `Assoc a -> parser_switch_state state UCL_STATE_READ_KEY
					| `List a -> parser_switch_state state UCL_STATE_READ_VALUE
					| _ -> raise (UCL_InternalError "Invalid object in stack")
				)
	in
	let rec parser_handle_after_value_helper state line is_quoted got_sep =
		if state.remain == 0 then
			parser_finish_def state line
		else (
			let c = line.[state.pos] in
			if Ucl_util.isspace_safe c then
				parser_handle_after_value_helper
					(parser_skip_chars Ucl_util.isspace_unsafe state line) line is_quoted got_sep
			else
				match c with
				| '}' -> (match List.hd state.stack with
							| `Assoc a ->
									parser_handle_after_value_helper (parser_pop_object_stack state line) line is_quoted true
							| _ -> raise (UCL_Syntax_Error ("Unmatched end of object", state)) )
				| ']' -> (match List.hd state.stack with
							| `List l ->
									parser_handle_after_value_helper (parser_pop_object_stack state line) line is_quoted true
							| _ -> raise (UCL_Syntax_Error ("Unmatched end of array", state)) )
				| ch ->
						if isvalueend ch then
							if parser_is_comment state line then
								parser_switch_state state UCL_STATE_COMMENT_START
							else
								parser_handle_after_value_helper (parser_next_char state line) line is_quoted true
						else
							(
								if got_sep || state.prev_state == UCL_STATE_COMMENT_START then
									parser_finish_def state line
								else
									raise (UCL_Syntax_Error ("Invalid character at the value end", state))
							)
		)
	in
	if state.remain > 0 then
		parser_handle_after_value_helper state line is_quoted false
	else
		parser_finish_def state line

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
			| UCL_STATE_AFTER_KEY ->
					parser_state_machine (parser_handle_after_key state line) line inx
			| UCL_STATE_READ_VALUE ->
					parser_state_machine (parser_handle_value state line) line inx
			| UCL_STATE_AFTER_VALUE_QUOTED ->
					parser_state_machine (parser_handle_after_value state line true) line inx
			| UCL_STATE_AFTER_VALUE_UNQUOTED ->
					parser_state_machine (parser_handle_after_value state line false) line inx
			| UCL_STATE_COMMENT_START | UCL_STATE_COMMENT_SINGLE | UCL_STATE_COMMENT_MULTI ->
					parser_state_machine (parser_handle_comment state line) line inx
			| UCL_STATE_END -> state
		in
		if state.remain > 0 then
			parser_parse_line state init
		else
			match state.state with
			| UCL_STATE_AFTER_VALUE_QUOTED ->
					parser_state_machine (parser_handle_after_value state init true) init inx
			| UCL_STATE_AFTER_VALUE_UNQUOTED ->
					parser_state_machine (parser_handle_after_value state init false) init inx
			| _ ->
					try
						let line = input_line inx in
						parser_parse_line { state with
								line = state.line + 1;
								column = 1;
								pos = 0;
								remain = String.length line; } line
					with End_of_file -> state
	in
	parser_state_machine state "" inx

let parse_in_channel inx =
	let init_state = {
		line = 0;
		column = 1;
		pos = 0;
		state = UCL_STATE_INIT;
		buf = None;
		key = None;
		value = None;
		stack = [];
		top = `Null;
		prev_state = UCL_STATE_INIT;
		remain = 0;
		comment_level = 0;
	} in
	let final_state = parser_parse_stream init_state inx in
	final_state.top

let parse_file filename =
	let inx = open_in filename in
	with_input_channel inx parse_in_channel