open Sexplib.Std
open Ftx
open Json_encoding

type channel =
  | Ticker
  | Trades
  | Orderbook
[@@deriving sexp]

let pp_print_channel ppf t =
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp) (sexp_of_channel t)

let channel_of_string = function
  | "ticker" -> Ticker
  | "trades" -> Trades
  | "orderbook" -> Orderbook
  | _ -> invalid_arg "channel_of_string"

let channel_encoding =
  string_enum [
    "ticker", Ticker ;
    "trades", Trades ;
    "orderbook", Orderbook ;
  ]

module Subscription = struct
  type t = {
    op: [`Subscribe | `Unsubscribe] ;
    channel: channel ;
    sym: string ;
  } [@@deriving sexp]

  let compare a b = Stdlib.compare a b
  let hash = Hashtbl.hash

  let subscribe channel sym = {
    op = `Subscribe ; channel ; sym }
  let unsubscribe channel sym = {
    op = `Unsubscribe ; channel ; sym }

  let encoding encoding =
    conv
      (fun { op ; channel ; sym } -> op, (channel, sym))
      (fun (op, (channel, sym)) -> { op ; channel ; sym })
      (merge_objs encoding
         (obj2
            (req "channel" channel_encoding)
            (req "market" string)))

  let sub_encoding =
    string_enum [
      "subscribe", `Subscribe ;
      "unsubscribe", `Unsubscribe ;
    ]

  let subbed_encoding =
    string_enum [
      "subscribed", `Subscribe ;
      "unsubscribed", `Unsubscribe ;
    ]

  let sub_encoding = encoding (obj1 (req "op" sub_encoding))
  let subbed_encoding = encoding (obj1 (req "type" subbed_encoding))
end

type msg = {
  code: int ;
  msg: string
} [@@deriving sexp]

let msg_encoding =
  conv
    (fun { code ; msg } -> ((), ( code, msg)))
    (fun ((), (code, msg)) -> { code ; msg })
    (merge_objs unit
       (obj2
          (req "code" int)
          (req "msg" string)))

type ticker = {
  bid : float option ;
  ask : float option ;
  last : float option ;
  ts : Ptime.t ;
} [@@deriving sexp]

let ticker_encoding =
  conv
    (fun { bid ; ask ; last ; ts } -> (bid, ask, last, ts))
    (fun (bid, ask, last, ts) -> { bid ; ask ; last ; ts })
    (obj4
       (req "bid" (option float))
       (req "ask" (option float))
       (req "last" (option float))
       (req "time" Ptime.encoding))

type quote = {
  price: float ;
  qty: float ;
} [@@deriving sexp]

let quote_encoding =
  conv
    (fun { price ; qty } -> price, qty)
    (fun (price, qty) -> { price ; qty })
    (tup2 float float)

let typ_encoding =
  string_enum [
    "partial", `Partial ;
    "update", `Update ;
  ]

module FloatMap = Map.Make(Float)

type book = {
  ts: Ptime.t ;
  chksum: float ;
  bids: quote list ;
  asks: quote list ;
  action: [`Partial | `Update]
} [@@deriving sexp]

let check_book ~bids ~asks =
  let buf = Buffer.create 13 in
  let add_float buf a =
    let frac, _ = Float.modf a in
    Buffer.add_string buf (Printf.sprintf "%F" a) ;
    if frac = 0. then Buffer.add_char buf '0' ;
    Buffer.add_char buf ':' in
  let rec chk bi b ai a =
    let b = if bi < 100 then b else FloatMap.empty in
    let a = if ai < 100 then a else FloatMap.empty in
    match FloatMap.max_binding_opt b, FloatMap.min_binding_opt a with
    | Some (pb, qb), Some (pa, qa) ->
      add_float buf pb ;
      add_float buf qb ;
      add_float buf pa ;
      add_float buf qa ;
      chk (succ bi) (FloatMap.remove pb b) (succ ai) (FloatMap.remove pa a)
    | Some (pb, qb), None ->
      add_float buf pb ;
      add_float buf qb ;
      chk (succ bi) (FloatMap.remove pb b) (succ ai) a
    | None, Some (pa, qa) ->
      add_float buf pa ;
      add_float buf qa ;
      chk (succ bi) b (succ ai) (FloatMap.remove pa a)
    | None, None -> ()
  in
  chk 0 bids 0 asks ;
  let prehash = Buffer.contents buf in
  Checkseum.Crc32.(digest_string prehash 0 (Buffer.length buf - 1) default)

let book_encoding =
  conv
    (fun _ -> assert false)
    (fun (ts, chksum, bids, asks, action) -> { ts ; chksum ; bids ; asks ; action })
    (obj5
       (req "time" Ptime.encoding)
       (req "checksum" float)
       (req "bids" (list quote_encoding))
       (req "asks" (list quote_encoding))
       (req "action" typ_encoding))

type trade = {
  id: int64 option ;
  ts: Ptime.t ;
  price: float ;
  size: float ;
  side: Fixtypes.Side.t ;
  liquidation: bool ;
} [@@deriving sexp]

let side_encoding =
  string_enum [
    "buy", Fixtypes.Side.Buy ;
    "sell", Sell ;
  ]

let trade_encoding =
  conv
    (fun { id ; ts ; price ; size ; side ; liquidation } ->
       (id, ts, price, size, side, liquidation))
    (fun (id, ts, price, size, side, liquidation) ->
       { id ; ts ; price ; size ; side ; liquidation })
    (obj6
       (req "id" (option int53))
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
  | Response of Subscription.t
  | Ticker of string * ticker
  | Quotes of string * book
  | Trades of string * trade list
[@@deriving sexp]

let error_encoding =
  conv
    (fun (code, err) -> (), code, err)
    (fun ((), code, err) -> (code, err))
    (obj3
       (req "type" (constant "error"))
       (req "code" int)
       (req "msg" string))

let info_encoding =
  conv
    (fun msg -> (), msg)
    (fun ((), msg) -> msg)
    (merge_objs (obj1 (req "type" (constant "info"))) msg_encoding)

let ticker_encoding =
  conv
    (fun (sym, ticker) -> ((), (), sym, ticker))
    (fun ((), (), sym, ticker) -> sym, ticker)
    (obj4
       (req "channel" (constant "ticker"))
       (req "type" (constant "update"))
       (req "market" string)
       (req "data" ticker_encoding))

let encoding =
  union [
    case error_encoding
      (function Error { code; msg } -> Some (code, msg) | _ -> None)
      (fun (code, msg) -> Error { code ; msg }) ;
    case info_encoding (function Info msg -> Some msg | _ -> None) (fun msg -> Info msg) ;
    case Subscription.subbed_encoding
      (function Response sub -> Some sub | _ -> None)
      (fun sub -> Response sub) ;

    case ticker_encoding
      (function Ticker (sym, ticker) -> Some (sym, ticker) | _ -> None)
      (fun (sym, ticker) -> Ticker (sym, ticker)) ;

    case (data_encoding book_encoding)
      (fun _ -> assert false)
      (fun { sym; data; _ } -> Quotes (sym, data)) ;

    case (data_encoding (list trade_encoding))
      (fun _ -> assert false)
      (fun { sym; data; _ } -> Trades (sym, data)) ;
  ]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
