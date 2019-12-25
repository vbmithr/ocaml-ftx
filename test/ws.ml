open Core
open Async

open Ftx_ws

let src = Logs.Src.create "ftx.ws-test"  ~doc:"Ftx API - WS test application"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let markets = ref []

let subs_r, subs_w = Pipe.create ()

let process_user_cmd w =
  let process s =
    let open Subscription in
    match String.split s ~on:' ' with
    | "all" :: _ ->
      Deferred.List.iter !markets ~f:begin fun sym ->
        Pipe.write w (subscribe Trades sym) >>= fun () ->
        begin Pipe.read subs_r >>= function
        | `Eof -> assert false
        | `Ok _ -> Deferred.unit
        end >>= fun () ->
        Pipe.write w (subscribe Orderbook sym) >>= fun () ->
        Pipe.read subs_r >>= function
        | `Eof -> assert false
        | `Ok _ -> Deferred.unit
      end
    | "tickers" :: _ ->
      Deferred.List.iter !markets ~f:begin fun sym ->
        Pipe.write w (subscribe Ticker sym) >>= fun () ->
        Pipe.read subs_r >>= function
        | `Eof -> assert false
        | `Ok _ -> Deferred.unit
      end
    | "alltrades" :: _ ->
      Deferred.List.iter !markets ~f:begin fun sym ->
        Pipe.write w (subscribe Trades sym) >>= fun () ->
        Pipe.read subs_r >>= function
        | `Eof -> assert false
        | `Ok _ -> Deferred.unit
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
      Log_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Log_async.err (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  Fastrest.request Ftx_rest.markets >>= function
  | Error e -> Error.raise e
  | Ok mkts ->
    markets := List.map mkts ~f:(fun { name; _ } -> name) ;
    Ftx_ws_async.with_connection_exn url ~f:begin fun r w ->
      let bks = String.Table.create () in
      let log_incoming msg =
        begin match msg with
          | Quotes (sym, { chksum; bids; asks; action = `Update; _ }) ->
            let bbook, abook = String.Table.find_exn bks sym in
            let bbook = List.fold_left bids ~init:bbook ~f:(fun a { price; qty } ->
                if qty = 0. then FloatMap.remove price a
                else FloatMap.add price qty a) in
            let abook = List.fold_left asks ~init:abook ~f:(fun a { price; qty } ->
                if qty = 0. then FloatMap.remove price a
                else FloatMap.add price qty a) in
            if not (check_book ~bids:bbook ~asks:abook = Optint.of_float chksum) then
              failwith "Checksum ERROR" ;
            String.Table.set bks ~key:sym ~data:(bbook, abook)
          | Response sub -> Pipe.write_without_pushback_if_open subs_w sub
          | Quotes (sym, { chksum; bids; asks; action = `Partial; _ }) ->
            let bids = List.fold_left ~init:FloatMap.empty
                ~f:(fun a {price; qty} -> FloatMap.add price qty a) bids in
            let asks = List.fold_left ~init:FloatMap.empty
                ~f:(fun a {price; qty} -> FloatMap.add price qty a) asks in
            begin match check_book ~bids ~asks = Optint.of_float chksum with
              | true -> Log.debug (fun m -> m "Checksum OK")
              | false ->             failwith "Checksum ERROR"
            end ;
            String.Table.set bks ~key:sym ~data:(bids, asks) ;
          | _ -> ()
        end ;
        Log_async.debug (fun m -> m "%a" pp msg) in
      Deferred.all_unit [
        process_user_cmd w ;
        Pipe.iter r ~f:log_incoming
      ]
    end

let () =
  Command.async ~summary:"Ftx WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
