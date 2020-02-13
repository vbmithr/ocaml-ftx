open Core
open Async
open Alcotest_async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.ignore_m
  end

let rest = [
  wrap_request "markets" Ftx_rest.markets ;
]

let main () =
  run "ftx" [
    "rest", rest ;
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())

