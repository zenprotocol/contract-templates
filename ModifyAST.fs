module ModifyAST

module AST   = FStar.Parser.AST
module Id    = FStar.Ident
module Const = FStar.Const
module Range = FStar.Range

type Comment    = string * FStar.Range.range
type AST        = AST.modul * list<Comment>
type paramsdata = ParametersData.paramsdata


let private update_range (s : string) (r : Range.range) : Range.range =
    let n = String.length s + 2
    let e = Range.end_of_range r
    let l = Range.line_of_pos e
    let c = Range.col_of_pos e + n
    Range.mk_range (Range.file_of_range r) (Range.start_of_range r) (Range.mk_pos l c) 

let private modify_term' (newdata : paramsdata) (t : AST.term') : AST.term' =
    match t with
    | AST.Const(Const.Const_bool(_))     ->
        match newdata with
        | ParametersData.CD_bool(b)               -> AST.Const(Const.Const_bool(b))
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_int(_,t))    ->
        match newdata with
        | ParametersData.CD_int(s,t') when t = t' -> AST.Const(Const.Const_int(s,t)) 
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_char(_))     ->
        match newdata with
        | ParametersData.CD_char(c)               -> AST.Const(Const.Const_char(c))
        | _                        -> failwith "TODO"
    | AST.Const(Const.Const_string(_,r)) ->
        match newdata with
        | ParametersData.CD_string(s)             -> AST.Const(Const.Const_string(s, update_range s r))
        | _                        -> failwith "TODO"
    | _                                  -> failwith "TODO"

let private modify_term (newdata : paramsdata) (t : AST.term) : AST.term =
    { t with tm = modify_term' newdata (t.tm) }

let private check_name (name : string) (p : AST.pattern) : bool =
    match p.pat with
    | AST.PatVar(v,_) -> v.idText = name
    | _               -> false

let private modify_pattern_term (name : string) (newdata : paramsdata) ((p,t) : AST.pattern * AST.term) : AST.pattern * AST.term =
    if check_name name p
        then (p, modify_term newdata t)
        else (p, t)

let private modify_decl' (name : string) (newdata : paramsdata) (dec' : AST.decl') : AST.decl' =
    match dec' with
    | AST.TopLevelLet(q, [pt]) -> AST.TopLevelLet(q, [modify_pattern_term name newdata pt])
    | _ -> dec'

let private modify_decl (name : string) (newdata : paramsdata) (dec : AST.decl) : AST.decl =
    { dec with d = modify_decl' name newdata dec.d }

let modify_modul (name : string) (newdata : paramsdata) (m : AST.modul) : AST.modul =
    match m with
    | AST.Module(lid, decs)       -> AST.Module(lid, List.map (modify_decl name newdata) decs) 
    | AST.Interface(lid, decs, b) -> AST.Interface(lid, List.map (modify_decl name newdata) decs, b)

let modify_AST ((m, cmts) : AST) ((name,newdata) : string * paramsdata) : AST =
    (modify_modul name newdata m, cmts)
 