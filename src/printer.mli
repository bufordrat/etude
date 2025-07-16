val show_to_printer :
  ('a -> string) -> Format.formatter -> 'a -> unit

val printer_to_show :
  (Format.formatter -> 'a -> unit) -> 'a -> string
