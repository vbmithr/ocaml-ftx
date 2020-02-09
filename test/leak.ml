open Core
open Async
open Ftx_ws

let main n =
  let module Encoding = Json_encoding.Make(Json_repr.Yojson) in
  let buf = Bi_outbuf.create 4096 in
  let of_string s =
    Encoding.destruct encoding (Yojson.Safe.from_string ~buf s) in
  let to_string t =
    Yojson.Safe.to_string ~buf (Encoding.construct encoding t) in
  let rec inner = function
    | 0 -> Deferred.unit
    | n when n > 0 ->
      Fastws_async.with_connection ~of_string ~to_string Ftx_ws.url begin fun _ _ ->
        Logs_async.app (fun m -> m "inner %d" n)
      end >>= fun () ->
      Clock_ns.after (Time_ns.Span.of_int_sec 3) >>= fun () ->
      inner (pred n)
    | _ -> invalid_arg "inner" in
  inner n

let () =
  Command.async ~summary:"Ftx leak test" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and n = anon ("n" %: int) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main n
    ] end |>
  Command.run
