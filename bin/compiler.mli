
type format = NONE | WANDA

type termination_technique = NONE | POLY

type config = {
  mutable format : format;
  mutable tt : termination_technique
}

val get_initial_config : unit -> config

val set_compiler : config -> unit

val compile : string -> string
