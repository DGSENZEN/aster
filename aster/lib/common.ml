type loc = {
  file : string option;
  line : int;
  column : int;
}

let dummy_loc = {
  file = None;
  line = 0;
  column = 0;
}

let make_loc ?file ~line ~column () = {
  file;
  line;
  column;
}

let loc_to_string loc =
  match loc.file with
  | Some file -> Printf.sprintf "%s:%d:%d" file loc.line loc.column
  | None -> Printf.sprintf "unknown:%d:%d" loc.line loc.column
