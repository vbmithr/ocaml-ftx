open Core
open Async

open Ftx
open Ftx_ws

let url = Uri.make ~scheme:"https" ~host:"ftx.com" ~path:"ws/" ()

let src = Logs.Src.create "ftx.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

let connect () =
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun cmd ->
        let doc =
          match Ezjsonm_encoding.construct Subscription.sub_encoding cmd with
          | `A _ | `O _ as a -> Ezjsonm.to_string a
          | _ -> invalid_arg "not a json document" in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      client_read, client_write, cleaned_up
    end

let connect_exn () =
  connect () >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection f =
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let r = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct Subscription.sub_encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    Monitor.protect
      (fun () -> f r client_write)
      ~finally:(fun () -> Pipe.close_read ws_read ; Deferred.unit)
  end

let with_connection_exn f =
  with_connection f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
