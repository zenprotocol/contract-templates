module Program

open Argu

open ModifyAST
open ResultUtil
open ViewFile

let ERR_MSG_UNSPECIFIED_CONTRACT =
    "Error - Contract file wasn't specified"
let ERR_MSG_FILE_NOT_FOUND : Printf.StringFormat<string -> string,string> =
    "Error - File not found:\n %s"
let ERR_MSG_PARSING_FAILED : Printf.StringFormat<string -> string,string> =
    "Error - Parsing failed for file:\n %s"
let ERR_MSG_INVALID_VIEW_FILE =
    "Error - Invalid view file"


type OutputStream =
    | Stdout
    | Stderr
    | File of filename:string

type Command_Write = {
    content : string
    stream  : OutputStream
}


type CLIArgs =
    | [<CliPrefix(CliPrefix.None)>]
        Extract of ParseResults<ExtractArgs>
    | [<CliPrefix(CliPrefix.None)>]
        Modify of ParseResults<ModifyArgs>
    | [<CliPrefix(CliPrefix.None)>]
        Generate of ParseResults<GenerateArgs>
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Extract(_)      -> "Extract parameters from a file to a .json view file"
            | Modify(_)       -> "Modify specific parameters in a file"
            | Generate(_)     -> "Generate a new contract from a contract template (contract + view files)"

and ExtractArgs =
    | [<MainCommand; ExactlyOnce>]
        Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>]
        V of view_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source_file(_) -> "Name of the contract (.fst) file to extract the parameters from"
            | V(_)           -> "Name of the view (.json) file to extract the parameters to.\n"
                              + "If this argument isn't specified the parameters will be printed to stdout"

and ModifyArgs =
    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--name")>]
        N of name:string
    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--value")>]
        V of value:string
    | [<MainCommand; ExactlyOnce>]
        Filenames of source_filename:string * destination_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Filenames(_,_) -> "Names of the source file and the generated file"
            | N(_)           -> "Parameter name"
            | V(_)           -> "Parameters value"

and GenerateArgs =
    | [<MainCommand; ExactlyOnce>]
        Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>]
        V of view_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source_file(_) -> "Name of the contract (.fst) file to use as template"
            | V(_)           -> "Name of the view (.json) file to take the parameters from.\n"
                              + "If this argument isn't specified the parameters will be taken from stdin"

let cli_parser      = ArgumentParser.Create<CLIArgs>()
let extract_parser  = ArgumentParser.Create<ExtractArgs>()
let modify_parser   = ArgumentParser.Create<ModifyArgs>()
let generate_parser = ArgumentParser.Create<GenerateArgs>()

let cli_usage()      = cli_parser.PrintUsage()
let extract_usage()  = extract_parser.PrintUsage()
let modify_usage()   = modify_parser.PrintUsage() 
let generate_usage() = generate_parser.PrintUsage() 

let with_file (filename : string) : Result<unit, string> =
     if System.IO.File.Exists filename
        then Result.Ok ()
        else Result.Error <| sprintf ERR_MSG_FILE_NOT_FOUND filename

let tryparse (filename : string) : Result<ASTUtils.AST, string> =
    try 
        Result.Ok <| ASTUtils.parse_file filename
    with _ ->
        Result.Error <| sprintf ERR_MSG_PARSING_FAILED filename  

let handle_extract_args (args : ParseResults<ExtractArgs>) : Result<Command_Write, string> =
    if args.GetAllResults() |> List.isEmpty
        then
            Result.Error <| extract_usage()
        else
            match args.TryGetResult ExtractArgs.Source_file with
            | None ->
                Result.Error ERR_MSG_UNSPECIFIED_CONTRACT
            | Some src_filename ->
                with_file src_filename >>= fun() ->
                result {
                    let  filename      = System.IO.Path.GetFileName src_filename
                    let  contract_name = System.IO.Path.GetFileNameWithoutExtension src_filename
                    let! ast           = tryparse src_filename
                    let  vf            = ViewFile.extractViewFile filename contract_name ast
                    let  json          = ViewFile.renderViewFile vf
                    return
                        { content = json
                        ; stream = 
                            match args.TryGetResult ExtractArgs.V with
                            | None ->
                                OutputStream.Stdout
                            | Some view_filename ->
                                OutputStream.File view_filename
                        }
                }


let handle_modify_args (args : ParseResults<ModifyArgs>) : Result<Command_Write, string> =
    Result.Error "TODO : Implement modify"

let handle_generate_args (args : ParseResults<GenerateArgs>) : Result<Command_Write, string> =
    if args.GetAllResults() |> List.isEmpty
        then
            Result.Error <| generate_usage()
        else
            result {
            let! content =
                match args.TryGetResult GenerateArgs.V with
                | None ->
                    Result.Ok <| System.Console.ReadLine()
                | Some view_filename ->
                    with_file view_filename >>= fun() ->
                    Result.Ok <| System.IO.File.ReadAllText view_filename
            
            let! vf =
                match parseViewFile content with
                | None ->
                    Result.Error ERR_MSG_INVALID_VIEW_FILE 
                | Some vf ->
                    Result.Ok vf
            
            let! src_filename =
                match args.TryGetResult GenerateArgs.Source_file with
                | None ->
                    Result.Error ERR_MSG_UNSPECIFIED_CONTRACT
                | Some src_filename ->
                    Result.Ok src_filename
            
            let! src_ast =
                tryparse src_filename
            
            let pars = vf._parameters
            
            let dst_ast =
                List.fold modify_AST src_ast pars
                
            let dst_filename = vf._filename
            
            return { content = ASTUtils.ast_to_string dst_ast; stream = OutputStream.File dst_filename }
            }

let handle_cli_arg arg =
    match arg with
    | Extract(args)  -> handle_extract_args  args
    | Modify(args)   -> handle_modify_args   args
    | Generate(args) -> handle_generate_args args

let rec handle_cli_args = List.fold (fun r hd -> handle_cli_arg hd :: r) [] >> sequenceResultM 

let cmd_write (cw : Command_Write) : unit =
    match cw.stream with
    | OutputStream.Stdout ->
        printfn "%s" cw.content
    | OutputStream.Stderr ->
        eprintfn "%s" cw.content
    | OutputStream.File filename  ->
         System.IO.File.WriteAllText(filename, cw.content)

let rec cmd_writeall (cws : Command_Write list) : unit =
    match cws with
    | []       -> ()
    | hd :: tl -> cmd_writeall tl; cmd_write hd

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then printfn "%s" (cli_parser.PrintUsage())
    try
        let results = cli_parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let args = results.GetAllResults()
        match handle_cli_args args with 
        | Result.Ok cws ->
            cmd_writeall cws
            0
        | Result.Error err ->
             printfn "%s" err
             1
    with e ->
        printfn "%s" e.Message
        1
