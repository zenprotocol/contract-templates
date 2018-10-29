
#r "bin/Debug/basic.dll"
#r "bin/Debug/parser.dll"
#r "bin/Debug/ZFS_Tools.dll"

module AST = FStar.Parser.AST
module Id = FStar.Ident
module Const = FStar.Const


let getDeclerations (ast : ASTUtils.AST) : list<AST.decl> =
    match fst ast with
    | AST.Module(_, decls)       -> decls
    | AST.Interface(_, decls, _) -> decls

let filterTLLs: list<AST.decl> -> list<list<AST.pattern * AST.term>> =
    List.choose (
        function
        | {AST.decl.d = AST.decl'.TopLevelLet(_, pts)} -> Some pts
        | _                                            -> None
    )

let filterSingletons : list<list<AST.pattern * AST.term>> -> list<AST.pattern' * AST.term'> =
    List.choose (
        function
        | [(p,t)] -> Some (p.pat, t.tm)
        | _       -> None
    )

let filterConstants : list<AST.pattern' * AST.term'> -> list<Id.ident * Const.sconst> =
    List.choose (
        function
        | (AST.PatVar (v, _), AST.Const c) -> Some (v, c)
        | _                                -> None 
    )

let getIdText : list<Id.ident * Const.sconst> -> list<string * Const.sconst> =
    List.map ( fun (id, e) -> (id.idText, e) )

let cvtConstData : Const.sconst -> ModifyAST.constdata =
    function
    | Const.Const_bool(b)         -> ModifyAST.CD_bool(b)
    | Const.Const_int(s,t)        -> ModifyAST.CD_int(s,t)
    | Const.Const_char(c)         -> ModifyAST.CD_char(c)
    | Const.Const_string(s,_)     -> ModifyAST.CD_string(s)
    | Const.Const_unit
    | Const.Const_float(_)
    | Const.Const_bytearray(_,_)
    | Const.Const_effect
    | Const.Const_range_of
    | Const.Const_set_range_of
    | Const.Const_range(_)
    | Const.Const_reify
    | Const.Const_reflect(_)      -> ModifyAST.CD_invalid

let cvtConstDatas : list<string * Const.sconst> -> list<string * ModifyAST.constdata> =
    List.map ( fun (s, e) -> (s, cvtConstData e) )

let extractData (ast : ASTUtils.AST) : list<string * ModifyAST.constdata> =
    ast |> getDeclerations
        |> filterTLLs
        |> filterSingletons
        |> filterConstants
        |> getIdText
        |> cvtConstDatas


(*
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
*)

let SRC_FILENAME = "../../zfc/contracts/Bet/Bet.fst"
let DST_FILENAME = "Bet2.fst"

//for arg in fsi.CommandLineArgs do
//    printfn "%s" arg

let ast = ASTUtils.parse_file SRC_FILENAME

printfn "%A" (extractData ast)

ModifyAST.modify_AST "ticker" (ModifyAST.CD_string "Intel") ast
    |> ASTUtils.write_ast_to_file DST_FILENAME
