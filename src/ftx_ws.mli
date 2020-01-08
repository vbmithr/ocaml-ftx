val url : Uri.t

type channel =
  | Ticker
  | Trades
  | Orderbook
[@@deriving sexp]

val pp_print_channel : Format.formatter -> channel -> unit
val channel_of_string : string -> channel
val channel_encoding : channel Json_encoding.encoding

module Subscription : sig
  type t = {
    channel: channel ;
    sym: string ;
  } [@@deriving sexp_of]

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
  module Table : Hashtbl.S with type key := t

  val compare : t -> t -> int
  val hash : t -> int

  val ticker : string -> t
  val trades : string -> t
  val books : string -> t
end

type msg = {
  code: int ;
  msg: string
}

type ticker = {
  bid : float option ;
  ask : float option ;
  bidSize: float option ;
  askSize: float option ;
  last : float option ;
  ts : Ptime.t ;
}

type 'a data = {
  typ: [ `Partial | `Update ] ;
  channel: channel ;
  sym: string ;
  data: 'a ;
} [@@deriving sexp]

type quote = {
  price: float ;
  qty: float ;
} [@@deriving sexp]

type book = {
  ts: Ptime.t ;
  chksum: float ;
  bids: quote list ;
  asks: quote list ;
  action : [ `Partial | `Update ] ;
} [@@deriving sexp]

module FloatMap : Map.S with type key = float
val check_book : bids:float FloatMap.t -> asks:float FloatMap.t -> Optint.t

type trade = {
  id: int64 option ;
  ts: Ptime.t ;
  price: float ;
  size: float ;
  side: Fixtypes.Side.t ;
  liquidation: bool ;
} [@@deriving sexp]

type t =
  | Error of { code: int ; msg: string }
  | Info of msg
  | Ping
  | Pong

  | Subscribe of Subscription.t
  | Unsubscribe of Subscription.t
  | Subscribed of Subscription.t
  | Unsubscribed of Subscription.t

  | Ticker of string * ticker
  | Quotes of string * book
  | Trades of string * trade list
[@@deriving sexp]

val ticker_sub : string -> t
val ticker_unsub : string -> t

val trades_sub : string -> t
val trades_unsub : string -> t

val books_sub : string -> t
val books_unsub : string -> t

val encoding : t Json_encoding.encoding
val pp : Format.formatter -> t -> unit
val of_string : string -> t
val to_string : t -> string
