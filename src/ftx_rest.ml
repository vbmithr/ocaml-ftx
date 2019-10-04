open Core
open Fastrest

let result_encoding encoding =
  let open Json_encoding in
  conv
    (fun _ -> assert false)
    (function (true, None, Some v) -> Ok v
            | (false, Some msg, None) -> Error (Error.of_string msg)
            | _ -> invalid_arg "result_encoding")
    (obj3
       (req "success" bool)
       (opt "error" string)
       (opt "result" encoding))

type market = {
  ask: float option ;
  bid: float option ;
  last: float option ;
  enabled: bool ;
  name: string ;
  priceIncrement: float ;
  sizeIncrement: float ;
} [@@deriving sexp]

let market_encoding =
  let open Json_encoding in
  conv
    (fun { ask ; bid ; last ; enabled ; name ; priceIncrement ; sizeIncrement } ->
       (), (ask, bid, last, enabled, name, priceIncrement, sizeIncrement))
    (fun ((), (ask, bid, last, enabled, name, priceIncrement, sizeIncrement)) ->
       { ask ; bid ; last ; enabled ; name ; priceIncrement ; sizeIncrement })
    (merge_objs unit
       (obj7
          (req "ask" (option float))
          (req "bid" (option float))
          (req "last" (option float))
          (req "enabled" bool)
          (req "name" string)
          (req "priceIncrement" float)
          (req "sizeIncrement" float)))

let base_url =
  Uri.make ~scheme:"https" ~host:"ftexchange.com" ()

let markets =
  let open Json_encoding in
  get (result_encoding (list market_encoding))
    (Uri.with_path base_url "api/markets")
