open Core
open Async

let rec inner = function
  | 0 -> Deferred.unit
  | n when n > 0 ->
    Ftx_ws_async.with_connection_exn
      Ftx_ws.url ~f:begin fun _r _w ->
      Logs_async.app (fun m -> m "inner %d" n)
    end >>= fun () ->
    Clock_ns.after (Time_ns.Span.of_int_sec 3) >>= fun () ->
    inner (pred n)
  | _ -> invalid_arg "inner"

let () =
  Command.async ~summary:"Ftx leak test" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and n = anon ("n" %: int) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        inner n
    ] end |>
  Command.run
