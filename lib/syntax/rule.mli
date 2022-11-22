open Term

type rule

type trs = rule list

val rule_mk : term -> term -> rule

val lhs : rule -> term

val rhs : rule -> term

val equal : rule -> rule -> bool

val to_string : rule -> string

