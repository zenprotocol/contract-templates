module ExtractParams

module AST  = FStar.Parser.AST
module Id   = FStar.Ident
module Const = FStar.Const

type paramsdata = ParametersData.paramsdata

let private getDeclerations (ast : ASTUtils.AST) : list<AST.decl> =
    match fst ast with
    | AST.Module(_, decls)       -> decls
    | AST.Interface(_, decls, _) -> decls

let private filterTLLs: list<AST.decl> -> list<list<AST.pattern * AST.term>> =
    List.choose (
        function
        | {AST.decl.d = AST.decl'.TopLevelLet(_, pts)} -> Some pts
        | _                                            -> None
    )

let private filterSingletons : list<list<AST.pattern * AST.term>> -> list<AST.pattern' * AST.term'> =
    List.choose (
        function
        | [(p,t)] -> Some (p.pat, t.tm)
        | _       -> None
    )

let private filterConstants : list<AST.pattern' * AST.term'> -> list<Id.ident * Const.sconst> =
    List.choose (
        function
        | (AST.PatVar (v, _), AST.Const c) -> Some (v, c)
        | _                                -> None 
    )

let private getIdText : list<Id.ident * Const.sconst> -> list<string * Const.sconst> =
    List.map ( fun (id, e) -> (id.idText, e) )

let private cvtConstData : Const.sconst -> paramsdata =
    function
    | Const.Const_bool(b)         -> ParametersData.CD_bool(b)
    | Const.Const_int(s,t)        -> ParametersData.CD_int(s,t)
    | Const.Const_char(c)         -> ParametersData.CD_char(c)
    | Const.Const_string(s,_)     -> ParametersData.CD_string(s)
    | Const.Const_unit
    | Const.Const_float(_)
    | Const.Const_bytearray(_,_)
    | Const.Const_effect
    | Const.Const_range_of
    | Const.Const_set_range_of
    | Const.Const_range(_)
    | Const.Const_reify
    | Const.Const_reflect(_)      -> ParametersData.CD_invalid

let private cvtConstDatas : list<string * Const.sconst> -> list<string * paramsdata> =
    List.map ( fun (s, e) -> (s, cvtConstData e) )

let extractData (ast : ASTUtils.AST) : list<string * paramsdata> =
    ast |> getDeclerations
        |> filterTLLs
        |> filterSingletons
        |> filterConstants
        |> getIdText
        |> cvtConstDatas
