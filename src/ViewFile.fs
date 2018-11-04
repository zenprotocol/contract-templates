module ViewFile

module Const = FStar.Const

open ParametersData

open FSharp.Data
open FSharp.Data.JsonExtensions

let KEY_FILENAME      = "filename"
let KEY_CONTRACT_NAME = "contract-name"
let KEY_PARAMETERS    = "parameters"
let KEY_NAME          = "name"
let KEY_TYPE          = "type"
let KEY_VALUE         = "value"

let TYPE_BOOL   = "bool"
let TYPE_CHAR   = "char"
let TYPE_STRING = "string"
let TYPE_INT    = "int"
let TYPE_INT8   = "int8"
let TYPE_INT16  = "int16"
let TYPE_INT32  = "int32"
let TYPE_INT64  = "int64"
let TYPE_UINT8  = "uint8"
let TYPE_UINT16 = "uint16"
let TYPE_UINT32 = "uint32"
let TYPE_UINT64 = "uint64"

let FMT_INT : Printf.StringFormat<(string -> string -> string)> =
    "%sint%s"
let FMT_UNSIGNED = "u"
let FMT_SIGNED   = ""

let VALUE_TRUE  = "true"
let VALUE_FALSE = "false"

let INVALID_FIELD = "INVALID"

type Parameter = {
    _name  : string;
    _value : string;
    _type  : string;
}

type ViewFile<'a> = {
    _filename      : string;
    _contract_name : string;
    _parameters    : 'a list;
}



let private paramsdata2valuetype (x : paramsdata) : string * string = 
    match x with
    | CD_bool(b) ->
        ( match b with
          | true -> VALUE_TRUE
          | false -> VALUE_FALSE
        , TYPE_BOOL
        )
    | CD_int(x, ogz) ->
        ( x
        , (match ogz with
          | None -> TYPE_INT
          | Some (g,z) ->
                let sign = 
                    match g with
                    | Const.Signed   -> FMT_SIGNED
                    | Const.Unsigned -> FMT_UNSIGNED
                let size =
                    match z with
                    | Const.Int8  -> "8"
                    | Const.Int16 -> "16"
                    | Const.Int32 -> "32"
                    | Const.Int64 -> "64"
                sprintf FMT_INT sign size
          )
        )
    | CD_char(c) ->
        ( sprintf "\"%s\"" (string c), TYPE_CHAR)
    | CD_string(s) ->
        ( sprintf "\"%s\"" s, TYPE_STRING)
    | CD_invalid -> (INVALID_FIELD, INVALID_FIELD)

let private sp2param ((name, pd) : string * paramsdata) : Parameter =
    let vt = paramsdata2valuetype pd
    { _name = name; _value = fst vt; _type = snd vt}

let extractViewFile (filename : string) (contract_name : string) (ast : ASTUtils.AST) : ViewFile<Parameter> = {
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
        //| x1::x2::xs -> sprintf "%s , %s" (renderer x1) (renderList' (x2::xs))
        | x1::x2::xs -> sprintf "%s,\n\t\t%s" (renderer x1) (renderList' (x2::xs))
    sprintf "[\n\t\t%s\n\t]" (renderList' ls)

let private renderParameter (param : Parameter) : string =
    let name_field  = sprintf "\"%s\" : \"%s\"" KEY_NAME  param._name
    let value_field = sprintf "\"%s\" : %s" KEY_VALUE param._value
    let type_field  = sprintf "\"%s\" : \"%s\"" KEY_TYPE  param._type
    sprintf "{ %s, %s, %s }" name_field value_field type_field

let renderViewFile (vf : ViewFile<Parameter>) : string =
    let filename_field      = sprintf "\"%s\" : \"%s\"" KEY_FILENAME      vf._filename
    let contract_name_field = sprintf "\"%s\" : \"%s\"" KEY_CONTRACT_NAME vf._contract_name
    let parameters_field    = sprintf "\"%s\" : %s"     KEY_PARAMETERS    (renderList renderParameter vf._parameters)
    //sprintf "{ %s, %s, %s}" filename_field contract_name_field parameters_field
    sprintf "{\n\t%s,\n\t%s,\n\t%s\n}"
        filename_field contract_name_field parameters_field

let private parseParameter (jv : JsonValue) : (string * paramsdata) option =
    let _name  = (jv?name).AsString()
    let _type  = jv.GetProperty(KEY_TYPE).AsString() 
    let _value = (jv?value).AsString()
    Option.map (fun _value -> _name, _value ) <|
    match _type with
    | t when t = TYPE_BOOL ->
        try
            Some <| CD_bool((jv?value).AsBoolean())
        with _ -> None
    | t when t = TYPE_CHAR ->
        if _value.Length = 1
            then Some <| CD_char(_value.Chars(0))
            else None
    | t when t = TYPE_STRING ->
        Some <| CD_string(_value)
    | t when t = TYPE_INT ->
        Some <| CD_int(_value, None)
    | t when t = TYPE_INT8 ->
        Some <| CD_int(_value, Some(Const.Signed, Const.Int8))
    | t when t = TYPE_INT16 ->
        Some <| CD_int(_value, Some(Const.Signed, Const.Int16))
    | t when t = TYPE_INT32 ->
        Some <| CD_int(_value, Some(Const.Signed, Const.Int32))
    | t when t = TYPE_INT64 ->
        Some <| CD_int(_value, Some(Const.Signed, Const.Int64))
    | t when t = TYPE_UINT8 ->
        Some <| CD_int(_value, Some(Const.Unsigned, Const.Int8))
    | t when t = TYPE_UINT16 ->
        Some <| CD_int(_value, Some(Const.Unsigned, Const.Int16))
    | t when t = TYPE_UINT32 ->
        Some <| CD_int(_value, Some(Const.Unsigned, Const.Int32))
    | t when t = TYPE_UINT64 ->
        Some <| CD_int(_value, Some(Const.Unsigned, Const.Int64))
    | _ ->
        None

let parseViewFile (json : string) : ViewFile<string * paramsdata> option =
    let jv = JsonValue.Parse json
    let _filename      = jv.GetProperty(KEY_FILENAME).AsString()
    let _contract_name = jv.GetProperty(KEY_CONTRACT_NAME).AsString()
    let _parameters    =
        jv.GetProperty(KEY_PARAMETERS).AsArray()
        |> List.ofArray
        |> List.map parseParameter
        |> FSharpx.Option.sequence
    let (>>=) x f = Option.bind f x
    _parameters >>= fun(_parameters) -> Some {
        _filename      = _filename
        _contract_name = _contract_name
        _parameters    = _parameters
    }