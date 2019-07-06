open Async

open Ftx_ws

val connect : unit ->
  (t Pipe.Reader.t * subscription Pipe.Writer.t * unit Deferred.t,
  [ `Internal of exn | `WS of Fastws_async.error ]) result Deferred.t

val connect_exn : unit ->
  (t Pipe.Reader.t * subscription Pipe.Writer.t * unit Deferred.t)
    Deferred.t

val with_connection :
  (t Pipe.Reader.t -> subscription Pipe.Writer.t -> 'a Deferred.t) ->
  ('a, [ `Internal of exn
       | `User_callback of exn
       | `WS of Fastws_async.error ]) result Deferred.t

val with_connection_exn :
  (t Pipe.Reader.t -> subscription Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
