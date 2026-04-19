type loc = {line: int; column: int; file: string option; }

val dummy_loc: loc
val loc_to_string: loc -> string