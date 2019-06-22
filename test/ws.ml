open Core
open Async

open Ftx_ws
open Ftx_ws_async

let src = Logs.Src.create "ftx.ws-test"
    ~doc:"Ftx API - WS test application"

let markets = ref []

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "all" :: _ ->
      Deferred.List.iter !markets ~f:begin fun sym ->
        Pipe.write w (subscribe Trades sym) >>= fun () ->
        Pipe.write w (subscribe Orderbook sym)
      end
    | "tickers" :: _ ->
      Deferred.List.iter !markets ~f:begin fun sym ->
        Pipe.write w (subscribe Ticker sym)
      end
    | "ticker" :: syms ->
      Deferred.List.iter syms ~f:begin fun sym ->
        Pipe.write w (subscribe Ticker sym)
      end
    | "trades" :: syms ->
      Deferred.List.iter syms ~f:begin fun sym ->
        Pipe.write w (subscribe Trades sym)
      end
    | "books" :: syms ->
      Deferred.List.iter syms ~f:begin fun sym ->
        Pipe.write w (subscribe Orderbook sym)
      end
    | "untrades" :: syms ->
      Deferred.List.iter syms ~f:begin fun sym ->
        Pipe.write w (unsubscribe Trades sym)
      end
    | "unbooks" :: syms ->
      Deferred.List.iter syms ~f:begin fun sym ->
        Pipe.write w (unsubscribe Orderbook sym)
      end
    (* | "unsubscribe" :: chanid :: _ ->
     *   let chanid = int_of_string chanid in
     *   Pipe.write w (Unsubscribe { chanid ; reqid = None })
     * | "ping" :: v :: _ ->
     *   Pipe.write w (Ping (int_of_string_opt v))
     * | "ping" :: _ ->
     *   Pipe.write w (Ping None)
     * | "trades" :: pair ->
     *   let pairs = List.map ~f:Pair.of_string_exn pair in
     *   Pipe.write w (Subscribe (trades pairs))
     * | "books" :: pair ->
     *   let pairs = List.map ~f:Pair.of_string_exn pair in
     *   Pipe.write w (Subscribe (book10 pairs)) *)
    | h :: _ ->
      Logs_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Logs_async.err ~src (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  Fastrest.request Ftx_rest.markets >>= function
  | Error e ->
    failwith Format.(asprintf "%a" (Fastrest.pp_print_error pp_print_string) e)
  | Ok mkts ->
    markets := List.map mkts ~f:(fun { name; _ } -> name) ;
    with_connection begin fun r w ->
      let log_incoming msg =
        Logs_async.debug ~src (fun m -> m "%a" pp msg) in
      Deferred.all_unit [
        process_user_cmd w ;
        Pipe.iter r ~f:log_incoming
      ]
    end

let () =
  Command.async ~summary:"Ftx WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
