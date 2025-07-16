val show_to_printer : (Format.formatter -> 'a -> unit) -> 'a -> string
val printer_to_show : ('a -> string) -> Format.formatter -> 'a -> unit
