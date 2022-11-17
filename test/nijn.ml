(* open Data.Input_file *)
open File.Wanda

let fnat = Name "Nat"
let fbool = Name "Bool"
let fnatnat = Arr (Name "Jose", Arr (fnat, fbool))

(* declare [nat -> nat; fnat] --> nat *)
let declaration = Dec (In [fnatnat], Out "Out")

let names_dec = StringSet.elements (names_of_tydec declaration)

let l = List.map Format.print_string names_dec;
