(*
  TODO: Review this interface for more generic names.
   For instance, there must be a function to generate new, fresh names.
*)
(*-----------------------------------------------------------------------------
  Abstract name symbols
-----------------------------------------------------------------------------*)
module type NAME = sig
  type t
  (** [t] is the type for inner representation of names *)

  val equal : t -> t -> bool
  (** [equal t1 t2] is [true] iff [t1 == t2] *)

  val symb_list : unit -> t list

  val get_symb_opt : string -> t option

  val register_name : string -> t

  val to_string : t -> string

  val compare : t -> t -> int

  exception DuplicatedName of string
end

(*-----------------------------------------------------------------------------
  Names as indexed integers
-----------------------------------------------------------------------------*)
module IndexedName () : NAME = struct
  exception DuplicatedName of string

  type t = int
  (** names are integers *)

  let compare = Int.compare

  let equal = Int.equal

  (* [names] is reference pointer to the list of names at certain index it is initialized as the empty list
  *)
  let names = ref []

  (* [idx_size] is the size of [names] *)
  let names_size = ref 0

  let symb_list _ = List.init !names_size Fun.id

  (* mem_idx str lst 0 is the index of the first element that equals str in lst *)
  let rec idx_of_name (name : string)
                      (name_lst : string list)
                      (idx : int)
                      : int =
    match name_lst with
    | [] -> idx
    | hd :: tl ->
      if String.equal name hd then
        idx
      else
        idx_of_name name tl (idx + 1)

  let get_symb_opt (name : string) =
    let idx = idx_of_name name !names 0 in
    if idx >= !names_size then
      None
    else
      Some (!names_size - 1 - idx)

  let register_name new_name =
    match (get_symb_opt new_name) with
    | None ->
      let n = !names_size in (
        names_size := n + 1;
        names := new_name :: !names;
        n
      )
    | Some _ ->
      raise (DuplicatedName (
        String.concat "" [
          "Cannot register the name: ";
          "'"; new_name; "'";
          " it is already in the stack!"]))
  let to_string sym = List.nth !names (!names_size -1 -sym)
end

module _ : Set.OrderedType = IndexedName ()
