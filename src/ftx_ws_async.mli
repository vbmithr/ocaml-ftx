open Async

open Ftx_ws

val connect : unit ->
  (t Pipe.Reader.t * subscription Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  (t Pipe.Reader.t -> subscription Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
