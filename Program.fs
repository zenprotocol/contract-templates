module Program

open Argu

let MSG_HELP = "Display this list of options."

type CLIArgs =
    | [<CliPrefix(CliPrefix.None)>] Extract  of ParseResults<ExtractArgs>
    | [<CliPrefix(CliPrefix.None)>] Modify   of ParseResults<ModifyArgs>
    | [<CliPrefix(CliPrefix.None)>] Generate of ParseResults<GenerateArgs>
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Extract(_)      -> "Extract parameters from a file to a .json view file"
            | Modify(_)       -> "Modify specific parameters in a file"
            | Generate(_)     -> "Generate a new contract from a contract template (contract + view files)"

and ExtractArgs =
    | [<MainCommand; ExactlyOnce; Last>]                      Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>] V           of view_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source_file(_) -> "Name of the contract (.fst) file to extract the parameters from"
            | V(_)           -> "Name of the view (.json) file to extract the parameters to.\n"
                              + "If this argument isn't specified the parameters will be printed to stdout"

and ModifyArgs =
    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--name")>]  N of name:string
    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--value")>] V of value:string
    | [<MainCommand; ExactlyOnce; Last>] Filenames of source_filename:string * destination_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Filenames(_,_) -> "Names of the source file and the generated file"
            | N(_)           -> "Parameter name"
            | V(_)           -> "Parameters value"

and GenerateArgs =
    | [<MainCommand; ExactlyOnce; Last>]                      Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>] V           of view_filename:string 
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

let cli_usage()      = cli_parser.PrintUsage()      |> printfn "%s"
let extract_usage()  = extract_parser.PrintUsage()  |> printfn "%s"
let modify_usage()   = modify_parser.PrintUsage()   |> printfn "%s"
let generate_usage() = generate_parser.PrintUsage() |> printfn "%s"

let parse (argv : string[]) =
    try 
        let results = cli_parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        results.GetAllResults() |> Some
    with
        | :? Argu.ArguParseException -> None

//let handle_extract_arg arg =
//    match arg with
//    | ExtractArgs.Source_file(src_filename) ->
//        let ast = ASTUtils.parse_file src_filename
//        let vf = ViewFile.extractViewFile src_filename ast
//        printfn "%s" (ViewFile.renderViewFile vf)
//        //printfn "Source file: %s" src_filename  
//    | ExtractArgs.V(view_filename)          -> printfn "View file: %s" view_filename
//
//let handle_extract_args' = List.fold (fun () -> handle_extract_arg) ()

let handle_extract_args (args : ParseResults<ExtractArgs>) =
    match args.TryGetResult ExtractArgs.Source_file with
    | None ->
        printf "Source file wasn't specified"
    | Some src_filename ->
        let ast  = ASTUtils.parse_file src_filename
        let vf   = ViewFile.extractViewFile src_filename ast
        let json = ViewFile.renderViewFile vf
        match args.TryGetResult ExtractArgs.V with
        | None ->
            printfn "%s" json
        | Some view_filename ->
            System.IO.File.WriteAllText(view_filename, json)

let handle_modify_arg arg =
    match arg with
    | ModifyArgs.Filenames(source_filename, destination_filename) ->
        printfn "Source file: %s, Destination file: %s" source_filename destination_filename
    | ModifyArgs.N(name) ->
        printfn "Parameter name: %s" name
    | ModifyArgs.V(value) ->
        printfn "Parameter value: %s" value

let handle_modify_args' = List.fold (fun () -> handle_modify_arg) ()

let handle_modify_args args =
    match args with
    | []   -> modify_usage()
    | _::_ -> handle_modify_args' args

let handle_generate_arg arg =
    match arg with
    | GenerateArgs.Source_file(src_filename) -> printfn "Source file: %s" src_filename  
    | GenerateArgs.V(view_filename)          -> printfn "View file: %s" view_filename

let handle_generate_args' = List.fold (fun () -> handle_generate_arg) ()

let handle_generate_args args =
    match args with
    | []   -> generate_usage()
    | _::_ -> handle_generate_args' args

let handle_cli_arg arg =
    match arg with
    | Extract(args)  -> printfn "Extract:"  ; handle_extract_args  args
    | Modify(args)   -> printfn "Modify:"   ; handle_modify_args   (args.GetAllResults())
    | Generate(args) -> printfn "Generate:" ; handle_generate_args (args.GetAllResults())

let rec handle_cli_args = List.fold (fun () -> handle_cli_arg) () 

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then printfn "%s" (cli_parser.PrintUsage())
    try
        let results = cli_parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let args = results.GetAllResults()
        handle_cli_args args
        //printfn "%A" args
    with e ->
        printfn "%s" e.Message
    0
