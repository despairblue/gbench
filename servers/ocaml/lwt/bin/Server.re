open Lib;

Lwt.async_exception_hook :=
  fun
  | Unix.Unix_error(error, func, arg) =>
    Printf.printf(
      "Client connection error %s: %s(%S)",
      Unix.error_message(error),
      func,
      arg,
    )
  | exn => Printf.printf("Unhandled exception occured");

let () = Lwt_main.run(Server.start_server());
