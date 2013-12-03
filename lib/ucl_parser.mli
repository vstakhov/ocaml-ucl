type ucl_state =
    UCL_STATE_INIT
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
  stack : Ucl.ucl list;
  top : Ucl.ucl;
}
val u_nl : int
val u_sp : int
val u_tab : int
val u_quot : int
val u_lbrack : int
val u_rbrack : int
val u_lbrace : int
val u_rbrace : int
val u_colon : int
val u_dot : int
val u_comma : int
val u_minus : int
val u_slash : int
val u_bslash : int
val u_times : int
val is_white_unsafe : char -> bool
val parser_new_object :
  ucl_parser_state ->
  ?skip_char:bool ->
  ?is_array:bool -> ?set_top:bool -> unit -> ucl_parser_state
val parser_handle_init : ucl_parser_state -> string -> ucl_parser_state
val parser_handle_key : 'a -> 'b -> 'a
val parser_handle_value : 'a -> 'b -> 'a
val parser_handle_after_value : 'a -> 'b -> 'a
val parser_handle_comment : 'a -> 'b -> 'a
val parser_state_machine : ucl_parser_state -> in_channel -> ucl_parser_state
val parse_in_channel : in_channel -> Ucl.ucl
val parse_file : string -> Ucl.ucl
