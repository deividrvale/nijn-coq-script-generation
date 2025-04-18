open Certificate

let cert_to_rocq (data : cert_data) =
  match data.cert with
  | Poly _ ->
    Poly.poly_to_coq data
  | Rem _ -> Rrem.rr_to_coq data
  | _ -> exit 1
