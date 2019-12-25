open Core
open Async

type t = {
  r: Ftx_ws.t Pipe.Reader.t ;
  w: Ftx_ws.Subscription.t Pipe.Writer.t ;
}

val connect :     Uri.t -> t Deferred.Or_error.t
val connect_exn : Uri.t -> t Deferred.t

val with_connection :
  f:(Ftx_ws.t Pipe.Reader.t ->
     Ftx_ws.Subscription.t Pipe.Writer.t -> 'a Deferred.t) ->
  Uri.t -> 'a Deferred.Or_error.t

val with_connection_exn :
  f:(Ftx_ws.t Pipe.Reader.t ->
     Ftx_ws.Subscription.t Pipe.Writer.t -> 'a Deferred.t) ->
  Uri.t -> 'a Deferred.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    (unit -> address Or_error.t Deferred.t) -> t
end
