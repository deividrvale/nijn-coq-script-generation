(** {0 Module {b Compiler}}
TODO write an introduction for this module.
*)

exception Invalid_config of string
(** [Invalid_config] is raised when trying to run {b Onijn} with a invalid configuration option.

Configurations are set by the record {!type:config}.
*)

type format = NONE | WANDA
(** The acceptable input formats. *)

type termination_technique = NONE | POLY
(** The termination techniques supported by {b Onijn}. *)

type config = {
  mutable format : format;
  mutable tt : termination_technique
}
(** The [config] record collects the possible configuration given to {b Onijn}.

{b Important!}
In order to invoke {!compile},
a configuration must be set with {!set_compiler}.
*)

val config_to_string : config -> string
(** [config_to_string cfg] is the written representation of the configuration record [cfg].
*)

val get_initial_config : unit -> config
(** [get_initial_config ()] returns the initial configuration
    for the compiler to run. *)

val set_compiler : config -> unit
(** [set_compiler cfg] sets the configuration defined in [cfg]
    in the compilation environment.*)

val compile : string -> string
(** [compile file] compiles [file] to a {b Coq proof string}.
It expects that [file] is a string reprsentation of a valid certificate,
which is formmated according to one of the formats in {!type:format}

@raise Invalid_config if {b Onijn}
    is configured with an invalid configuration record.
*)
