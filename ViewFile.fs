module ViewFile

module Const = FStar.Const

open ParametersData

let KEY_FILENAME      = "filename"
let KEY_CONTRACT_NAME = "contract-name"
let KEY_PARAMETERS    = "parameters"
let KEY_NAME          = "name"
let KEY_TYPE          = "type"
let KEY_VALUE         = "value"

type Parameter = {
    _name  : string;
    _value : string;
    _type  : string;
}

type ViewFile = {
    _filename      : string;
    _contract_name : string;
    _parameters    : Parameter list;
}



let private paramsdata2valuetype (x : paramsdata) : string * string = 
    match x with
    | CD_bool(b) ->
        ( match b with
          | true -> "true"
          | false -> "false"
        , "bool"
        )
    | CD_int(x, ogz) ->
        ( x
        , (match ogz with
          | None -> "int"
          | Some (g,z) ->
                let sign = 
                    match g with
                    | Const.Signed -> ""
                    | Const.Unsigned -> "u"
                let size =
                    match z with
                    | Const.Int8  -> "8"
                    | Const.Int16 -> "16"
                    | Const.Int32 -> "32"
                    | Const.Int64 -> "64"
                sprintf "%sint%s" sign size
          )
        )
    | CD_char(c) ->
        ( sprintf "\"%s\"" (string c), "char")
    | CD_string(s) ->
        ( sprintf "\"%s\"" s, "string")
    | CD_invalid -> ("INVALID", "INVALID")

let private sp2param ((name, pd) : string * paramsdata) : Parameter =
    let vt = paramsdata2valuetype pd
    { _name = name; _value = fst vt; _type = snd vt}

let extractViewFile (filename : string) (contract_name : string) (ast : ASTUtils.AST) : ViewFile = {
    _filename      = filename;
    _contract_name = contract_name
    _parameters    =
        ExtractParams.extractData ast
        |> List.map sp2param
}

let private renderList (renderer : 'a -> string) (ls : 'a list) : string =
    let rec renderList' xs' =
        match xs' with
        | []         -> ""
        | [x]        -> renderer x
        | x1::x2::xs -> sprintf "%s , %s" (renderer x1) (renderList' (x2::xs))
    sprintf "[%s]" (renderList' ls)

let private renderParameter (param : Parameter) : string =
    let name_field  = sprintf "\"%s\" : \"%s\"" KEY_NAME  param._name
    let value_field = sprintf "\"%s\" : %s" KEY_VALUE param._value
    let type_field  = sprintf "\"%s\" : \"%s\"" KEY_TYPE  param._type
    sprintf "{ %s, %s, %s }" name_field value_field type_field

let renderViewFile (vf : ViewFile) : string =
    let filename_field      = sprintf "\"%s\" : \"%s\"" KEY_FILENAME      vf._filename
    let contract_name_field = sprintf "\"%s\" : \"%s\"" KEY_CONTRACT_NAME vf._contract_name
    let parameters_field    = sprintf "\"%s\" : %s" KEY_PARAMETERS    (renderList renderParameter vf._parameters)
    sprintf "{ %s, %s, %s}" filename_field contract_name_field parameters_field
