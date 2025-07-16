let test1 () =
  let open Alcotest in
  check int "one-is-one" 1 1

let () =
  let open Alcotest in
  run "Dude"
    [ ( "Test Group 1",
        [ test_case "The Test of One Being One" `Quick test1
        ] )
    ]
