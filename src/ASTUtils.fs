module ASTUtils

open System
open FStar.Parser.AST
open FSharpx.Functional.Prelude

module PP = FStar.Pprint
module TD = FStar.Parser.ToDocument

type Comment = string * FStar.Range.range
type AST = modul * list<Comment>

let string_of_doc: PP.document -> string =
    PP.pretty_string 1.0 100

let parse_file (filename: string) : AST =
    FStar.Parser.Driver.parse_file filename

let modul_to_string: modul -> string =
    TD.modul_to_document >> string_of_doc

let ast_to_doc : AST -> PP.document =
    uncurry TD.modul_with_comments_to_document
    >> fst

let ast_to_string: AST -> string =
    ast_to_doc >> string_of_doc

// prints a module to a specified output channel
let print_modul' : modul -> FStar.Util.out_channel -> unit =
    TD.modul_to_document >> PP.pretty_out_channel 1.0 100

// prints a module to stdout
let print_modul (m:modul) : unit =
    print_modul' m stdout

// prints an AST to a specified output channel
let print_ast' : AST -> FStar.Util.out_channel -> unit =
    ast_to_doc
    >> PP.pretty_out_channel 1.0 100

// prints an AST to stdout
let print_ast : AST -> unit =
    flip print_ast' stdout

let write_ast_to_file (filename:string): AST -> unit =
    ast_to_string
    >> curry IO.File.WriteAllText filename
