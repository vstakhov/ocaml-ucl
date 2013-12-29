open Ucl

(** Try ... Finally emulation *)
let unwind ~protect f x =
  let module E = struct type 'a t = Left of 'a | Right of exn end in
  let res = try E.Left (f x) with e -> E.Right e in
  let ()  = protect x in
  match res with
  | E.Left  y -> y
  | E.Right e -> raise e

let with_input_channel inch f =
  unwind ~protect:close_in f inch
	
(** Character sets function *)
let make_cset set =
	let a = Array.make 256 false in
	for i = 0 to String.length set - 1 do
		a.(Char.code set.[i]) <- true
	done;
	a

let in_cset set c = set.(Char.code c)

let lower	= "abcdefghijklmnopqrstuvxyz"
and upper	= "ABCDEFGHIJKLMNOPQRSTUVXYZ"
and digit	= "0123456789"

let alpha = make_cset (lower ^ upper)
and alnum = make_cset (lower ^ upper ^ digit)
and blank = make_cset " \t"
and cntrl = (let a = Array.make 256 false in
		for i =0 to 31 do
			a.(i) <- true;
		done;
		a.(127) <- true;
		a)
and digit = make_cset digit
and graph = (let a = Array.make 256 false in
		for i =33 to 126 do
			a.(i) <- true;
		done;
		a)
and lower = make_cset lower
and upper = make_cset upper
and punct = make_cset "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
and print = (let a = Array.make 256 false in
		for i =32 to 126 do
			a.(i) <- true;
		done;
		(* Assume non-ASCII characters as printable *)
		for i = 127 to 255 do
			a.(i) <- true;
		done;
		a)
and space_unsafe = make_cset " \t\r\n\014\013"
and space_safe = make_cset "\t "
and xdigit = make_cset (digit ^ "abcdefABCDEF")
and keystart = make_cset (lower ^ upper ^ digit ^ "/_")
and valueend = make_cset "\r\n]};,#"

let isalpha c = alpha.(Char.code c)
let isalnum c = alnum.(Char.code c)
let isblank c = blank.(Char.code c)
let iscntrl c = cntrl.(Char.code c)
let isdigit c = digit.(Char.code c)
let isgraph c = graph.(Char.code c)
let islower c = lower.(Char.code c)
let isupper c = upper.(Char.code c)
let ispunct c = punct.(Char.code c)
let isprint c = print.(Char.code c)
let isspace_safe c = space_safe.(Char.code c)
let isspace_unsafe c = space_unsafe.(Char.code c)
let isxdigit c = xdigit.(Char.code c)
let iskeystart c = keystart.(Char.code c)
let isvalueend c = valueend.(Char.code c)