let show_to_printer to_string p x =
  let open Format in
  pp_print_string p (to_string x)

let printer_to_show pp x =
  let open Format in
  pp str_formatter x ;
  flush_str_formatter ()
