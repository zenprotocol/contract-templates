module WitheredAST
(*
      + ============================= +
      | Withered Abstract Syntax Tree |
      + ============================= +

   A variant of the AST which doesn't contain any range information or parameters value information.
   Allows comparison of ASTs modulo parameter values.
   
*)

module AST   = FStar.Parser.AST
module Id    = FStar.Ident
module Const = FStar.Const
module Range = FStar.Range

type path = list<string>

type ident = string

type lid = {
    ns:list<ident>;
    ident:ident;
    nsstr:string;
    str:string
}

type sconst =
    | Const_effect
    | Const_unit
    | Const_bool        of bool
    | Const_int         of string * option<(Const.signedness * Const.width)>
    | Const_char        of char
    | Const_float       of double
    | Const_bytearray   of array<byte>
    | Const_string      of string
    | Const_range_of
    | Const_set_range_of
    | Const_range
    | Const_reify
    | Const_reflect     of lid

type sconst__ =
    | Const_bool__
    | Const_int__ of option<(Const.signedness * Const.width)>
    | Const_char__
    | Const_string__

type term' =
    | Wild
    | Const     of sconst
    | Const__   of sconst__
    | Op        of ident * list<term>
    | Tvar      of ident
    | Uvar      of ident
    | Var       of lid
    | Name      of lid
    | Projector of lid * ident
    | Construct of lid * list<(term*imp)>
    | Abs       of list<pattern> * term
    | App       of term * term * imp
    | Let       of AST.let_qualifier * list<(option<attributes_> * (pattern * term))> * term
    | LetOpen   of lid * term
    | Seq       of term * term
    | Bind      of pattern * term * term
    | IfBind    of term * term * term
    | If        of term * term * term
    | Match     of term * list<branch>
    | TryWith   of term * list<branch>
    | Ascribed  of term * term * option<term>
    | Record    of option<term> * list<(lid * term)>
    | Project   of term * lid
    | Product   of list<binder> * term
    | Sum       of list<binder> * term
    | QForall   of list<binder> * list<list<term>> * term
    | QExists   of list<binder> * list<list<term>> * term
    | Refine    of binder * term
    | NamedTyp  of ident * term
    | Paren     of term
    | Requires  of term * option<string>
    | Ensures   of term * option<string>
    | Labeled   of term * string * bool
    | Discrim   of lid
    | Attributes of list<term>
    | Antiquote of term
    | Quote     of term * AST.quote_kind
    | VQuote    of term

and term = {tm:term'; level:AST.level}

and attributes_ = list<term>

and binder' =
    | Variable of ident
    | TVariable of ident
    | Annotated of ident * term
    | TAnnotated of ident * term
    | NoName of term

and binder = {b:binder'; blevel:AST.level; aqual:aqual}

and pattern =
    | PatWild     of option<arg_qualifier>
    | PatConst    of sconst
    | PatApp      of pattern * list<pattern>
    | PatVar      of ident * option<arg_qualifier>
    | PatName     of lid
    | PatTvar     of ident * option<arg_qualifier>
    | PatList     of list<pattern>
    | PatVector   of list<pattern>
    | PatTuple    of list<pattern> * bool
    | PatRecord   of list<(lid * pattern)>
    | PatAscribed of pattern * (term * option<term>)
    | PatOr       of list<pattern>
    | PatOp       of ident

and branch = (pattern * option<term> * term)
and arg_qualifier =
    | Implicit
    | Equality
    | Meta of term
and aqual = option<arg_qualifier>
and imp =
    | FsTypApp
    | Hash
    | UnivApp
    | HashBrace of term
    | Nothing

type knd = term
type typ = term
type expr = term

type fsdoc = string * list<(string * string)>

type tycon =
    | TyconAbstract of ident * list<binder> * option<knd>
    | TyconAbbrev   of ident * list<binder> * option<knd> * term
    | TyconRecord   of ident * list<binder> * option<knd> * list<(ident * term * option<fsdoc>)>
    | TyconVariant  of ident * list<binder> * option<knd> * list<(ident * option<term> * option<fsdoc> * bool)>

type qualifiers = list<AST.qualifier>

type decoration =
    | Qualifier of AST.qualifier
    | DeclAttributes of list<term>
    | Doc of fsdoc

type lift_op =
    | NonReifiableLift of term
    | ReifiableLift    of term * term
    | LiftForFree      of term

type lift = {
    msource: lid;
    mdest:   lid;
    lift_op: lift_op;
}

type decl' =
    | TopLevelModule of lid
    | Open of lid
    | Friend of lid
    | Include of lid
    | ModuleAbbrev of ident * lid
    | TopLevelLet of AST.let_qualifier * list<(pattern * term)>
    | Main of term
    | Tycon of bool * bool * list<(tycon * option<fsdoc>)>
    | Val of ident * term
    | Exception of ident * option<term>
    | Pragma of AST.pragma
    | Fsdoc of fsdoc
    | Assume of ident * term
    | Splice of list<ident> * term

and decl = {
    d:decl';
    doc:option<fsdoc>;
    quals: qualifiers;
    attrs: attributes_
}

type modul =
    | Module of lid * list<decl>
    | Interface of lid * list<decl> * bool





(* ------------------------------------------------------------------------------------------------------------------ *)
(* ---------------------------------------- CONVERSION FUNCTIONS ---------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------------------------------ *)

let cvt_ident (id : Id.ident) : ident = id.idText

let cvt_lid (lid : Id.lid) : lid = {
    ns    = List.map cvt_ident lid.ns;
    ident = cvt_ident lid.ident;
    nsstr = lid.nsstr;
    str   = lid.str;
}

let cvt_sconst (sc : Const.sconst) : sconst =
    match sc with
    | Const.Const_effect
         -> Const_effect
    | Const.Const_unit
         -> Const_unit
    | Const.Const_bool(b)
         -> Const_bool(b)
    | Const.Const_int(s,t)
         -> Const_int(s,t)
    | Const.Const_char(c)
         -> Const_char(c)
    | Const.Const_float(x)
         -> Const_float(x)
    | Const.Const_bytearray(xs,_)
         -> Const_bytearray(xs)
    | Const.Const_string(s,_)
         -> Const_string(s)
    | Const.Const_range_of
         -> Const_range_of
    | Const.Const_set_range_of
         -> Const_set_range_of
    | Const.Const_range(_)
         -> Const_range
    | Const.Const_reify
         -> Const_reify
    | Const.Const_reflect(lid)
         -> Const_reflect(cvt_lid lid)    

let rec cvt_term' (t : AST.term') : term' =
    match t with
    | AST.Wild
       -> Wild
    | AST.Const(sc)
       -> Const(cvt_sconst sc)
    | AST.Op(id, terms)
       -> Op(cvt_ident id, List.map cvt_term terms)
    | AST.Tvar(id)
       -> Tvar(cvt_ident id)
    | AST.Uvar(id)
       -> Uvar(cvt_ident id)
    | AST.Var(lid)
       -> Var(cvt_lid lid)
    | AST.Name(lid)
       -> Name(cvt_lid lid)
    | AST.Projector(lid, id)
       -> Projector(cvt_lid lid, cvt_ident id)
    | AST.Construct(lid, tis)
       -> Construct(cvt_lid lid, List.map (fun (t,i) -> (cvt_term t, cvt_imp i)) tis)
    | AST.Abs(pats, term)
       -> Abs(List.map cvt_pattern pats, cvt_term term)
    | AST.App(t1, t2, i)
       -> App(cvt_term t1, cvt_term t2, cvt_imp i)
    | AST.Let(q, xs, t) ->
          Let(q, List.map (fun (ots,(p',t')) -> (Option.map (List.map cvt_term) ots, (cvt_pattern p', cvt_term t'))) xs, cvt_term t)
    | AST.LetOpen(lid, t)
       -> LetOpen(cvt_lid lid, cvt_term t)
    | AST.Seq(t1, t2)
       -> Seq(cvt_term t1, cvt_term t2)
    | AST.Bind(p, t1, t2)
       -> Bind(cvt_pattern p, cvt_term t1, cvt_term t2)
    | AST.IfBind(t1, t2, t3)
       -> IfBind(cvt_term t1, cvt_term t2, cvt_term t3) 
    | AST.If(t1, t2, t3)
       -> If(cvt_term t1, cvt_term t2, cvt_term t3)
    | AST.Match(t, bs)
       -> Match(cvt_term t, List.map cvt_branch bs)
    | AST.TryWith(t, bs)
       -> TryWith(cvt_term t, List.map cvt_branch bs)
    | AST.Ascribed(t1, t2, ot)
       -> Ascribed(cvt_term t1, cvt_term t2, Option.map cvt_term ot)
    | AST.Record(ot, lts)
       -> Record(Option.map cvt_term ot, List.map (fun (lid,t) -> (cvt_lid lid, cvt_term t)) lts)
    | AST.Project(t,lid)
       -> Project(cvt_term t, cvt_lid lid)
    | AST.Product(bs, t)
       -> Product(List.map cvt_binder bs, cvt_term t)
    | AST.Sum(bs, t)
       -> Sum(List.map cvt_binder bs, cvt_term t)
    | AST.QForall(bs, tss, t)
       -> QForall(List.map cvt_binder bs, List.map (List.map cvt_term) tss, cvt_term t)
    | AST.QExists(bs, tss, t)
       -> QExists(List.map cvt_binder bs, List.map (List.map cvt_term) tss, cvt_term t)
    | AST.Refine(b,t)
       -> Refine(cvt_binder b, cvt_term t)
    | AST.NamedTyp(id, t)
       -> NamedTyp(cvt_ident id, cvt_term t)
    | AST.Paren(t)
       -> Paren(cvt_term t)
    | AST.Requires(t, os)
       -> Requires(cvt_term t, os)
    | AST.Ensures(t, os)
       -> Ensures(cvt_term t, os)
    | AST.Labeled(t, s, b)
       -> Labeled(cvt_term t, s, b)
    | AST.Discrim(lid)
       -> Discrim(cvt_lid lid)
    | AST.Attributes(ts)
       -> Attributes(List.map cvt_term ts)
    | AST.Antiquote(t)
       -> Antiquote(cvt_term t)
    | AST.Quote(t,k)
       -> Quote(cvt_term t,k)
    | AST.VQuote(t)
       -> VQuote(cvt_term t)

and cvt_term (t : AST.term) : term =
    {tm=cvt_term' t.tm; level=t.level}
     
and cvt_binder' (b : AST.binder') : binder' =
     match b with
     | AST.Variable(id)
        -> Variable(cvt_ident id)
     | AST.TVariable(id)
        -> TVariable(cvt_ident id)
     | AST.Annotated(id,t)
        -> Annotated(cvt_ident id, cvt_term t)
     | AST.TAnnotated(id,t)
        -> TAnnotated(cvt_ident id, cvt_term t)
     | AST.NoName(t)
        -> NoName(cvt_term t)

and cvt_binder (b : AST.binder) : binder =
    {b=cvt_binder' b.b; blevel=b.blevel; aqual=Option.map cvt_arg_qualifier b.aqual}

and cvt_arg_qualifier (aq : AST.arg_qualifier) : arg_qualifier =
    match aq with
    | AST.Implicit
       -> Implicit 
    | AST.Equality
       -> Equality
    | AST.Meta(t)
       -> Meta(cvt_term t)

and cvt_branch ((p,ot,t) : AST.branch) : branch =
    (cvt_pattern p, Option.map cvt_term ot, cvt_term t)

and cvt_pattern (p : AST.pattern) : pattern =
    match p.pat with
    | AST.PatWild(oaq)
       -> PatWild(Option.map cvt_arg_qualifier oaq)
    | AST.PatConst(sc)
       -> PatConst(cvt_sconst sc)
    | AST.PatApp(p, ps)
       -> PatApp(cvt_pattern p, List.map cvt_pattern ps) 
    | AST.PatVar(id, oaq)
       -> PatVar(cvt_ident id, Option.map cvt_arg_qualifier oaq)
    | AST.PatName(lid)
       -> PatName(cvt_lid lid)
    | AST.PatTvar(id, oaq)
       -> PatTvar(cvt_ident id, Option.map cvt_arg_qualifier oaq)
    | AST.PatList(ps)
       -> PatList(List.map cvt_pattern ps)
    | AST.PatVector(ps)
       -> PatVector(List.map cvt_pattern ps)
    | AST.PatTuple(ps,b)
       -> PatTuple(List.map cvt_pattern ps, b)
    | AST.PatRecord(lps)
       -> PatRecord(List.map (fun (l,p) -> (cvt_lid l, cvt_pattern p)) lps)
    | AST.PatAscribed(p, (t, ot))
       -> PatAscribed(cvt_pattern p, (cvt_term t, Option.map cvt_term ot))
    | AST.PatOr(ps)
       -> PatOr(List.map cvt_pattern ps)
    | AST.PatOp(id)
       -> PatOp(cvt_ident id)

and cvt_imp (i : AST.imp) : imp =
    match i with
    | AST.FsTypApp
       -> FsTypApp
    | AST.Hash
       -> Hash
    | AST.UnivApp
       -> UnivApp
    | AST.HashBrace(t)
       -> HashBrace(cvt_term t)
    | AST.Nothing
       -> Nothing

let cvt_tycon (t : AST.tycon) : tycon =
    match t with
    | AST.TyconAbstract(id, bs, ok)
       -> TyconAbstract(cvt_ident id, List.map cvt_binder bs, Option.map cvt_term ok)
    | AST.TyconAbbrev(id, bs, ok, t)
       -> TyconAbbrev(cvt_ident id, List.map cvt_binder bs, Option.map cvt_term ok, cvt_term t)
    | AST.TyconRecord(id, bs, ok, idtofss)
       -> TyconRecord(cvt_ident id, List.map cvt_binder bs, Option.map cvt_term ok,
                      List.map (fun (id', t', ofs) -> (cvt_ident id', cvt_term t', ofs)) idtofss)
    | AST.TyconVariant(id, bs, ok, idotofssb)
       -> TyconVariant(cvt_ident id, List.map cvt_binder bs, Option.map cvt_term ok,
                      List.map (fun (id', ot', ofs, b) -> (cvt_ident id', Option.map cvt_term ot', ofs, b)) idotofssb)

let cvt_sconst__ (c : Const.sconst) : sconst__ =
    match c with
    | Const.Const_bool(_)
         -> Const_bool__
    | Const.Const_int(_,t)
         -> Const_int__(t)
    | Const.Const_char(_)
         -> Const_char__
    | Const.Const_string(_,_)
         -> Const_string__
    | Const.Const_float(_)
    | Const.Const_bytearray(_,_)
    | Const.Const_effect
    | Const.Const_unit
    | Const.Const_range_of
    | Const.Const_set_range_of
    | Const.Const_range(_)
    | Const.Const_reify
    | Const.Const_reflect(_)
         -> failwith "Invalid parameter type"

let cvt_decl' (d : AST.decl') : decl' =
    match d with
    
    //Sorry about this mess
    | AST.TopLevelLet(lq, [(p,t)])
           when (match p.pat with | AST.PatVar(_,_) when (match t.tm with | AST.Const(_) -> true | _ -> false) -> true | _ -> false)
           -> match p.pat with
              | AST.PatVar(_,_) ->
                  match t.tm with
                  | AST.Const(c) -> TopLevelLet(lq, [(cvt_pattern p, {tm=Const__(cvt_sconst__ c); level=t.level})])
                  | _ -> failwith "This should never happen"
              | _ -> failwith "This should never happen"
    
    | AST.TopLevelModule(lid)
       -> TopLevelModule(cvt_lid lid)
    | AST.Open(lid)
       -> Open(cvt_lid lid)
    | AST.Friend(lid)
       -> Friend(cvt_lid lid)
    | AST.Include(lid)
       -> Include(cvt_lid lid)
    | AST.ModuleAbbrev(id,lid)
       -> ModuleAbbrev(cvt_ident id, cvt_lid lid)
    | AST.TopLevelLet(lq, pts)
       -> TopLevelLet(lq, List.map (fun (p,t) -> (cvt_pattern p, cvt_term t)) pts)
    | AST.Main(t)
       -> Main(cvt_term t)
    | AST.Tycon(b1, b2, tycfs)
       -> Tycon(b1, b2, List.map (fun (tyc, ofs) -> (cvt_tycon tyc, ofs)) tycfs)
    | AST.Val(id, t)
       -> Val(cvt_ident id, cvt_term t)
    | AST.Exception(id, ot)
       -> Exception(cvt_ident id, Option.map cvt_term ot)
    | AST.NewEffect(_) -> failwith "ZF* doesn't support effect declerations (NewEffect)"
    | AST.SubEffect(_) -> failwith "ZF* doesn't support effect declerations (SubEffect)"
    | AST.Pragma(p)
       -> Pragma(p)
    | AST.Fsdoc(fsd)
       -> Fsdoc(fsd)
    | AST.Assume(id,t)
       -> Assume(cvt_ident id, cvt_term t)
    | AST.Splice(ids, t)
       -> Splice(List.map cvt_ident ids, cvt_term t)

let cvt_decl (d : AST.decl) : decl = {
    d     = cvt_decl' d.d;
    doc   = d.doc;
    quals = d.quals;
    attrs = List.map cvt_term d.attrs;
}

let cvt_modul (m : AST.modul) : modul =
    match m with
    | AST.Module(lid,decs)      -> Module(cvt_lid lid, List.map cvt_decl decs)
    | AST.Interface(lid,decs,b) -> Interface(cvt_lid lid, List.map cvt_decl decs, b)
