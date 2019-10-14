open Sexplib.Std

let strfloat =
  let open Json_encoding in
  union [
    case float (fun s -> Some s) (fun s -> s) ;
    case string (fun s -> Some (string_of_float s)) float_of_string ;
  ]

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 ~frac_s:7 t)

  let unix_encoding =
    let open Json_encoding in
    conv
      Ptime.to_float_s
      (fun ts -> match Ptime.of_float_s ts with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      float

  let rfc3339_encoding =
    let open Json_encoding in
    conv
      (Ptime.to_rfc3339)
      (fun s -> match Ptime.of_rfc3339 s with
         | Ok (v, _, _) -> v
         | _ -> failwith "rfc3339_encoding")
      string

  let encoding =
    let open Json_encoding in
    union [
      case unix_encoding (fun a -> Some a) (fun a -> a) ;
      case rfc3339_encoding (fun a -> Some a) (fun a -> a) ;
    ]
end
