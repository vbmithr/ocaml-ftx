open Fastrest

type secTyp =
  | Spot of { base: string; quote: string }
  | Future of { underlying: string } [@@deriving sexp]

type stats = {
  ask: float;
  bid: float;
  last: float option;
  price: float option;
  change1h: float option;
  change24h: float option;
  changeBod: float option;
  quoteVolume24h: float option;
  volumeUsd24h: float option;
} [@@deriving sexp]

type market = {
  name: string ;
  secTyp: secTyp ;
  stats: stats ;
  enabled: bool ;
  priceIncrement: float ;
  sizeIncrement: float ;
} [@@deriving sexp]

val markets : (form, market list) service
