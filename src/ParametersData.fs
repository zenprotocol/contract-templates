module ParametersData

module Const = FStar.Const

type paramsdata =
    | CD_bool   of bool
    | CD_int    of string * option<(Const.signedness * Const.width)> (* When None, means "mathematical integer", i.e. Prims.int. *)
    | CD_char   of char                                              (* unicode code point: char in F#, int in OCaml *)
    | CD_string of string                                            (* UTF-8 encoded *)
    | CD_invalid

