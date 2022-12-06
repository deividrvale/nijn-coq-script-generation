(** {0 Module {b Compiler}}
*)

type format = NONE | WANDA
(** The acceptable input formats. *)

type termination_technique = NONE | POLY
(** The termination techniques supported by {b Onijn}. *)

type config = {
  mutable format : format;
  mutable tt : termination_technique
}
(** The [config] record collects the possible configuration given to onijn.
In order to call [!compile], a configuration must be set with [!set_compiler].
*)

val get_initial_config : unit -> config
(** [get_initial_config ()] returns the initial configuration
    for the compiler to run. *)

val set_compiler : config -> unit
(**  *)

val compile : string -> string
(** [compile file] compiles [file] to a {b Coq proof string}.
    It expects that [file] is a string reprsentation of a valid certificate,
    which is formmated according to one of the formats in {!type:format} *)
