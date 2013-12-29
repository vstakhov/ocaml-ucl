val unwind : protect:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
val with_input_channel : in_channel -> (in_channel -> 'a) -> 'a
val make_cset : string -> bool array
val in_cset : 'a array -> char -> 'a
val alpha : bool array
val alnum : bool array
val blank : bool array
val cntrl : bool array
val digit : bool array
val graph : bool array
val lower : bool array
val upper : bool array
val punct : bool array
val print : bool array
val space_unsafe : bool array
val space_safe : bool array
val xdigit : bool array
val keystart : bool array
val valueend : bool array
val isalpha : char -> bool
val isalnum : char -> bool
val isblank : char -> bool
val iscntrl : char -> bool
val isdigit : char -> bool
val isgraph : char -> bool
val islower : char -> bool
val isupper : char -> bool
val ispunct : char -> bool
val isprint : char -> bool
val isspace_safe : char -> bool
val isspace_unsafe : char -> bool
val isxdigit : char -> bool
val iskeystart : char -> bool
val isvalueend : char -> bool
