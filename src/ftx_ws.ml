open Sexplib.Std
open Ftx

type channel =
  | Ticker
  | Trades
  | Orderbook
[@@deriving sexp]

let channel_of_string = function
  | "ticker" -> Ticker
  | "trades" -> Trades
  | "orderbook" -> Orderbook
  | _ -> invalid_arg "channel_of_string"

let channel_encoding =
  let open Json_encoding in
  string_enum [
    "ticker", Ticker ;
    "trades", Trades ;
    "orderbook", Orderbook ;
  ]

type subscription = {
  op: [`Subscribe | `Unsubscribe] ;
  channel: channel ;
  sym: string ;
}

let subscribe channel sym = {
  op = `Subscribe ; channel ; sym }
let unsubscribe channel sym = {
  op = `Unsubscribe ; channel ; sym }

let subscription_encoding encoding =
  let open Json_encoding in
  conv
    (fun { op ; channel ; sym } -> op, (channel, sym))
    (fun (op, (channel, sym)) -> { op ; channel ; sym })
    (merge_objs encoding
       (obj2
          (req "channel" channel_encoding)
          (req "market" string)))

let sub_encoding =
  let open Json_encoding in
  subscription_encoding
    (obj1 (req "op" ((string_enum ["subscribe", `Subscribe ;
                                   "unsubscribe", `Unsubscribe]))))

type msg = {
  code: int ;
  msg: string
} [@@deriving sexp]

let msg_encoding =
  let open Json_encoding in
  conv
    (fun { code ; msg } -> ((), ( code, msg)))
    (fun ((), (code, msg)) -> { code ; msg })
    (merge_objs unit
       (obj2
          (req "code" int)
          (req "msg" string)))

type ticker = {
  bid : float ;
  ask : float ;
  last : float ;
  ts : Ptime.t ;
} [@@deriving sexp]

let ticker_encoding =
  let open Json_encoding in
  conv
    (fun { bid ; ask ; last ; ts } -> (bid, ask, last, ts))
    (fun (bid, ask, last, ts) -> { bid ; ask ; last ; ts })
    (obj4
       (req "bid" float)
       (req "ask" float)
       (req "last" float)
       (req "time" Ptime.encoding))

type quote = {
  price: float ;
  qty: float ;
} [@@deriving sexp]

let quote_encoding =
  let open Json_encoding in
  conv
    (fun { price ; qty } -> price, qty)
    (fun (price, qty) -> { price ; qty })
    (tup2 float float)

let typ_encoding =
  let open Json_encoding in
  string_enum [
    "partial", `Partial ;
    "update", `Update ;
  ]

type book = {
  ts: Ptime.t ;
  chksum: int64 ;
  bids: quote list ;
  asks: quote list ;
  action: [`Partial | `Update]
} [@@deriving sexp]

let book_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; chksum ; bids ; asks ; action } -> (ts, chksum, bids, asks, action))
    (fun (ts, chksum, bids, asks, action) -> { ts ; chksum ; bids ; asks ; action })
    (obj5
       (req "time" Ptime.encoding)
       (req "checksum" int53)
       (req "bids" (list quote_encoding))
       (req "asks" (list quote_encoding))
       (req "action" typ_encoding))

type trade = {
  id: int option ;
  ts: Ptime.t ;
  price: float ;
  size: float ;
  side: [`Buy | `Sell] ;
  liquidation: bool ;
} [@@deriving sexp]

let side_encoding =
  let open Json_encoding in
  string_enum [
    "buy", `Buy ;
    "sell", `Sell ;
  ]

let trade_encoding =
  let open Json_encoding in
  conv
    (fun { id ; ts ; price ; size ; side ; liquidation } ->
       (id, ts, price, size, side, liquidation))
    (fun (id, ts, price, size, side, liquidation) ->
       { id ; ts ; price ; size ; side ; liquidation })
    (obj6
       (req "id" (option int))
       (req "time" Ptime.encoding)
       (req "price" float)
       (req "size" float)
       (req "side" side_encoding)
       (req "liquidation" bool))

type 'a data = {
  typ: [ `Partial | `Update ] ;
  channel: channel ;
  sym: string ;
  data: 'a ;
} [@@deriving sexp]

let data_encoding encoding =
  let open Json_encoding in
  conv
    (fun { typ ; channel ; sym ; data } -> (typ, channel, sym, data))
    (fun (typ, channel, sym, data) -> { typ ; channel ; sym ; data })
    (obj4
       (req "type" typ_encoding)
       (req "channel" channel_encoding)
       (req "market" string)
       (req "data" encoding))

type t =
  | Error of { code: int ; msg: string }
  | Info of msg
  | Subscribed of channel * string
  | Unsubscribed of channel * string
  | Ticker of string * ticker
  | Book of book data
  | Trade of trade list data
[@@deriving sexp]

let error_encoding =
  let open Json_encoding in
  conv
    (fun (code, err) -> (), code, err)
    (fun ((), code, err) -> (code, err))
    (obj3
       (req "type" (constant "error"))
       (req "code" int)
       (req "msg" string))

let info_encoding =
  let open Json_encoding in
  conv
    (fun msg -> (), msg)
    (fun ((), msg) -> msg)
    (merge_objs (obj1 (req "type" (constant "info"))) msg_encoding)

let ticker_encoding =
  let open Json_encoding in
  conv
    (fun (sym, ticker) -> ((), (), sym, ticker))
    (fun ((), (), sym, ticker) -> sym, ticker)
    (obj4
       (req "channel" (constant "ticker"))
       (req "type" (constant "update"))
       (req "market" string)
       (req "data" ticker_encoding))

let encoding =
  let open Json_encoding in
  union [
    case error_encoding
      (function Error { code; msg } -> Some (code, msg) | _ -> None)
      (fun (code, msg) -> Error { code ; msg }) ;
    case info_encoding (function Info msg -> Some msg | _ -> None) (fun msg -> Info msg) ;
    case (subscription_encoding
            (obj1 (req "type" (string_enum ["subscribed", `Subscribe ;
                                            "unsubscribed", `Unsubscribe]))))
      (function
        | Subscribed (channel, sym) -> Some { op = `Subscribe ; channel ; sym }
        | Unsubscribed (channel, sym) -> Some { op = `Unsubscribe ; channel ; sym }
        | _ -> None)
      (fun { op ; channel; sym } ->
         match op with
         | `Subscribe -> Subscribed (channel, sym)
         | `Unsubscribe -> Unsubscribed (channel, sym)
      ) ;

    case ticker_encoding
      (function Ticker (sym, ticker) -> Some (sym, ticker) | _ -> None)
      (fun (sym, ticker) -> Ticker (sym, ticker)) ;

    case (data_encoding book_encoding)
      (function Book b -> Some b | _ -> None)
      (fun b -> Book b) ;

    case (data_encoding (list trade_encoding))
      (function Trade t -> Some t | _ -> None)
      (fun t -> Trade t) ;
  ]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
