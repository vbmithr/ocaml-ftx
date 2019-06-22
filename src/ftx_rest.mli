open Fastrest

type market = {
  ask: float option ;
  bid: float option ;
  last: float option ;
  enabled: bool ;
  name: string ;
  priceIncrement: float ;
  sizeIncrement: float ;
} [@@deriving sexp]

val markets : (get, market list, string) service
