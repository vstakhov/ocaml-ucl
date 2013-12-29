open Ucl

type ucl_state =
	| UCL_STATE_INIT
	| UCL_STATE_READ_KEY
	| UCL_STATE_READ_VALUE
	| UCL_STATE_AFTER_VALUE
	| UCL_STATE_COMMENT
	| UCL_STATE_END

type ucl_parser_state = {
	line : int;
	column : int;
	pos : int;
	state : ucl_state;
	prev_state : ucl_state;
	buf : Buffer.t;
	stack : ucl list;
	top : ucl;
	remain : int;
}

exception UCL_Syntax_Error of (string * ucl_parser_state)

val parse_in_channel : in_channel -> Ucl.ucl
val parse_file : string -> Ucl.ucl