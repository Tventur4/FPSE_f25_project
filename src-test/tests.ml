[@@@ocaml.warning "-33"]

open Core
open OUnit2

let series =
  "Src Testing" >::: 
  [ Cards_tests.series 
  ; Table_tests.series
  ; Round_tests.series]

let () = run_test_tt_main series