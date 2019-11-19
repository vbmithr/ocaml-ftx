open Fastrest

type secTyp =
  | Spot of { base: string; quote: string }
  | Future of { underlying: string } [@@deriving sexp]

type stats = {
  ask: float;
  bid: float;
  last: float option;
  price: float;
  change1h: float;
  change24h: float;
  changeBod: float;
  quoteVolume24h: float;
  volumeUsd24h: float;
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
