type channel =
  | Ticker
  | Trades
  | Orderbook

val channel_of_string : string -> channel
val channel_encoding : channel Json_encoding.encoding

type subscription = {
  op: [`Subscribe | `Unsubscribe] ;
  channel: channel ;
  sym: string ;
}

val sub_encoding : subscription Json_encoding.encoding

val subscribe : channel -> string -> subscription
val unsubscribe : channel -> string -> subscription

type msg = {
  code: int ;
  msg: string
}

type ticker = {
  bid : float ;
  ask : float ;
  last : float ;
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
  chksum: int64 ;
  bids: quote list ;
  asks: quote list ;
  action: [ `Partial | `Update ] ;
} [@@deriving sexp]

type t =
  | Info of msg
  | Subscribed of channel * string
  | Unsubscribed of channel * string
  | Ticker of string * ticker
  | Book of book data
[@@deriving sexp]

val encoding : t Json_encoding.encoding
val pp : Format.formatter -> t -> unit
