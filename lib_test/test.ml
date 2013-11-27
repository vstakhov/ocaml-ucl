Open Ucl

let () =
	Command.basic ~summary:"Parse and display UCL"
		Command.Spec.(empty +> anon ("filename" %: file))
		ucl
	|> Command.run