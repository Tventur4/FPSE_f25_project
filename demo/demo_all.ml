open Lwt.Syntax
open Lwt.Infix 

(* defining main function *)
let main () =

  (*OWL DEMO*)
  Lwt_io.printl "\nOWL DEMO\n" >>= fun () ->
  let _m = Owl.Mat.of_arrays [| (*defining a two dimensional array with owl.*)
    [| 1.; 2. |];
    [| 3.; 4. |]
  |] in
  
  (*LWT DEMO*)
  Lwt_io.printl "\nLWT DEMO\n" >>= fun () ->
  (*in addition to printl, printf, async function*)
  let* lwt_result = Lwt.return "Lwt promise resolved!" in
  Lwt_io.printl lwt_result >>= fun () ->

  Lwt_io.printl "\nAll demos successful."

(* Run the main Lwt event loop *)
let () =
  Lwt_main.run (main ())