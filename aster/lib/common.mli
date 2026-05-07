type loc = {
  file : string option;
  line : int;
  column : int;
}

val dummy_loc : loc
val make_loc : ?file:string -> line:int -> column:int -> unit -> loc
val loc_to_string : loc -> string
