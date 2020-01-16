open Core
open Fastrest
open Json_encoding

let result_encoding encoding =
  conv
    (fun _ -> assert false)
    (function (true, None, Some v) -> Ok v
            | (false, Some msg, None) -> Error (Error.of_string msg)
            | _ -> invalid_arg "result_encoding")
    (obj3
       (req "success" bool)
       (opt "error" string)
       (opt "result" encoding))

type secTyp =
  | Spot of { base: string; quote: string }
  | Future of { underlying: string } [@@deriving sexp]

let typ_encoding =
  string_enum [
    "spot", `Spot;
    "future", `Future;
  ]

let secTyp_encoding =
  conv
    (fun _ -> assert false)
    (fun (t, b, q, u) ->
       match t with
       | `Future -> (match u with Some underlying -> Future { underlying } | _ -> invalid_arg "secType")
       | `Spot -> (match b,q with Some base, Some quote -> Spot { base; quote } | _ -> invalid_arg "secType")
    )
    (obj4
       (req "type" typ_encoding)
       (req "baseCurrency" (option string))
       (req "quoteCurrency" (option string))
       (req "underlying" (option string)))

type stats = {
  ask: float option ;
  bid: float option ;
  last: float option;
  price: float option;
  change1h: float option;
  change24h: float option;
  changeBod: float option;
  quoteVolume24h: float option;
  volumeUsd24h: float option;
} [@@deriving sexp]

let stats_encoding =
  conv
    (fun _ -> assert false)
    (fun (ask, bid, last, price, change1h, change24h, changeBod, quoteVolume24h, volumeUsd24h) ->
       { ask ; bid ; last ; price ; change1h ; change24h ; changeBod ; quoteVolume24h ; volumeUsd24h })
    (obj9
       (req "ask" (option float))
       (req "bid" (option float))
       (req "last" (option float))
       (req "price" (option float))
       (opt "change1h" float)
       (opt "change24h" float)
       (opt "changeBod" float)
       (opt "quoteVolume24h" float)
       (opt "volumeUsd24h" float))

type market = {
  name: string ;
  secTyp: secTyp ;
  stats: stats ;
  enabled: bool ;
  priceIncrement: float ;
  sizeIncrement: float ;
} [@@deriving sexp]

let market_encoding =
  conv
    (fun _ -> assert false)
    (fun ((secTyp, stats), (enabled, name, priceIncrement, sizeIncrement)) ->
       { secTyp; stats; enabled ; name ; priceIncrement ; sizeIncrement })
    (merge_objs (merge_objs secTyp_encoding stats_encoding)
       (obj4
          (req "enabled" bool)
          (req "name" string)
          (req "priceIncrement" float)
          (req "sizeIncrement" float)))

let base_url =
  Uri.make ~scheme:"https" ~host:"ftx.com" ()

let markets =
  get (result_encoding (list market_encoding))
    (Uri.with_path base_url "api/markets")
