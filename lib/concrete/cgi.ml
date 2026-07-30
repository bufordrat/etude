let crlf = "\r\n"

let success_response body =
  let headers =
    [ "Status: 200"; "Content-Type: text/plain" ]
  in
  String.concat ""
    [ String.concat crlf headers; crlf; crlf; body; crlf ]
