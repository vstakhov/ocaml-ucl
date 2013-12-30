val unwind : protect:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
val with_input_channel : in_channel -> (in_channel -> 'a) -> 'a
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
