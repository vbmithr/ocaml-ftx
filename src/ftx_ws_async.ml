open Core
open Async

open Ftx
open Ftx_ws

let src = Logs.Src.create "ftx.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

module T = struct
  type t = {
    r: Ftx_ws.t Pipe.Reader.t ;
    w: Subscription.t Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit
  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let mk_client_read r =
  Pipe.map r ~f:begin fun msg ->
    Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
  end

let mk_client_write w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct Subscription.sub_encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end
  end

let connect url =
  Deferred.Or_error.map (Fastws_async.connect url)
    ~f:begin fun { r; w; _ } ->
      let client_write = mk_client_write w in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      create (mk_client_read r) client_write
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay =
    create ~server_name ?on_event ?retry_delay ~connect
end

let connect_exn url =
  connect url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ~f url =
  Fastws_async.with_connection url ~f:begin fun r w ->
    f (mk_client_read r) (mk_client_write w)
  end

let with_connection_exn ~f url =
  with_connection ~f url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
