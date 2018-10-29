module Program

open Argu

type FilenameArg =
    | FAS of string
    | FAT of string * string * string

type CLIArgs =
    | [<CliPrefix(CliPrefix.None)>] Extract of ParseResults<ExtractArgs>
    | [<CliPrefix(CliPrefix.None)>] Modify  of ParseResults<ModifyArgs>
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Extract(_)      -> "Extract parameters from a file"
            | Modify(_)       -> "Modify parameters in a file" 

and ExtractArgs =
    | [<MainCommand; ExactlyOnce; Last>] Src_Filename of string
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Src_Filename(_)  -> "File name of the contract to extract parameters from"

and ModifyArgs =
    | [<Mandatory>] [<CliPrefix(CliPrefix.Dash)>] [<AltCommandLine("--name")>]  N of string
    | [<Mandatory>] [<CliPrefix(CliPrefix.Dash)>] [<AltCommandLine("--value")>] V of string
    | [<MainCommand; ExactlyOnce; Last>] Src_Filename of string
    | [<MainCommand; ExactlyOnce; Last>] Dst_Filename of filename:string
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Src_Filename(_)  -> "File name of the contract to modify"
            | Dst_Filename(_) -> "File name of the destination modified contract"
            | N(_) -> "Parameter name"
            | V(_) -> "Parameters value"

let parser = ArgumentParser.Create<CLIArgs>()

let usage() = parser.PrintUsage() 

let parse (argv : string[]) =
    try
        let results = parser.Parse argv
        results.GetAllResults() |> Some
    with
        | :? Argu.ArguParseException -> None

[<EntryPoint>]
let main argv =
    try 
        parser.ParseCommandLine(inputs = argv, raiseOnUsage = true) |> ignore
    with e ->
        printfn "%s" e.Message
    0 
    //printfn "%s" (usage())
//    match parse argv with
//    | Some res -> printfn "%A" res            ; 0
//    | None     -> printfn "Invalid arguments" ; 1
