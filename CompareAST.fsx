
#r "bin/Debug/basic.dll"
#r "bin/Debug/parser.dll"
#r "bin/Debug/ZFS_Tools.dll"

module AST = FStar.Parser.AST

type Comment = string * FStar.Range.range
type AST = AST.modul * list<Comment>

let compareASTs ((m1,_) : AST) ((m2,_) : AST) : bool =
     WitheredAST.cvt_modul m1 = WitheredAST.cvt_modul m2

(*
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
*)

let SRC_FILENAME = "../../zfc/contracts/Bet/Bet.fst"
//let DST_FILENAME = "Bet2.fst"

//for arg in fsi.CommandLineArgs do
//    printfn "%s" arg

let ast1 = ASTUtils.parse_file SRC_FILENAME

let ast2 = ModifyAST.modify_AST "ticker" (ModifyAST.CD_string "Intel") ast1
// ASTUtils.write_ast_to_file DST_FILENAME

compareASTs ast1 ast2 |> printfn "equal ASTs? : %A"
