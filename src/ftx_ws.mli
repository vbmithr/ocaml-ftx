type channel =
  | Ticker
  | Trades
  | Orderbook
[@@deriving sexp]

val pp_print_channel : Format.formatter -> channel -> unit
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
  bid : float option ;
  ask : float option ;
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
  chksum: int64 ;
  bids: quote list ;
  asks: quote list ;
  action: [ `Partial | `Update ] ;
} [@@deriving sexp]

val check_book : bids:quote list -> asks:quote list -> int32
(** bids and asks must be in best order *)

type trade = {
  id: int option ;
  ts: Ptime.t ;
  price: float ;
  size: float ;
  side: [`Buy | `Sell] ;
  liquidation: bool ;
} [@@deriving sexp]

type t =
  | Error of { code: int ; msg: string }
  | Info of msg
  | Subscribed of channel * string
  | Unsubscribed of channel * string
  | Ticker of string * ticker
  | BookSnapshot of string * book
  | Quotes of string * book
  | Trades of string * trade list
[@@deriving sexp]

val encoding : t Json_encoding.encoding
val pp : Format.formatter -> t -> unit
