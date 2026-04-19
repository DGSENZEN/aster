type loc = {line: int; column: int; file: string option}

let dummy_loc: loc = {line = 0; column = 0; file = None;}

let loc_to_string loc = match loc.file with
  | Some name -> Printf.sprintf "%s:%i:%i" name loc.line loc.column
  | None -> Printf.sprintf "unknown:0:0"