let show_to_printer to_string p x = Format.pp_print_string p (to_string x)
let printer_to_show pp x = pp Format.str_formatter x ; Format.flush_str_formatter ()
