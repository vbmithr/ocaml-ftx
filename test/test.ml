open Core
open Async

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Info)

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.ignore_m
  end

let rest = [
  wrap_request "markets" Ftx_rest.markets ;
]

let () =
  Alcotest.run ~and_exit:false "ftx" [
    "rest", rest ;
  ]

