open System.IO
open Fantomas.Core
open FSharpLint.Framework
open FSharpLint.Application
open Ignore
open Defacto

let config: FormatConfig =
    { FormatConfig.Default with
        SpaceBeforeUppercaseInvocation = false
        SpaceBeforeLowercaseInvocation = false
        MaxIfThenElseShortWidth = 120
        MaxInfixOperatorExpression = 120
        KeepMaxNumberOfBlankLines = 3
        MultilineBracketStyle = Aligned
        NewlineBetweenTypeDefinitionAndMembers = true
        MultiLineLambdaClosingNewline = true
        ExperimentalKeepIndentInBranch = true
        MaxRecordNumberOfItems = 3
        RecordMultilineFormatter = NumberOfItems
    }

let format_source_code_async (source_text: string) : Async<string> =
    async {
        let! source_text_fixed = SyntaxTreeFormatting.find_and_apply_fixes(source_text)

        let! ast_array = CodeFormatter.ParseOakAsync(false, source_text_fixed)
        let ast, _ = ast_array.[0]
        return! CodeFormatter.FormatOakAsync(ast, config)
    }

let check_file (file: string) : Async<Result<Message list, string * string>> =
    async {
        let! text = Async.AwaitTask(File.ReadAllTextAsync(file))
        let! formatted = format_source_code_async(text)

        let messages =
            if text <> formatted then [ { Id = DF0001; FilePath = file; Location = None } ] else []

        let messages =
            RegexCheckWarnings.find_matches(file, text) |> List.ofSeq |> List.append(messages)

        let messages =
            BannedSymbolCheckWarnings.find_matches(file, text) |> List.ofSeq |> List.append(messages)

        return messages
    }
    |> Async.Catch
    |> Async.map(
        function
        | Choice1Of2 messages -> Ok messages
        | Choice2Of2 err -> Error(file, err.Message)
    )

let format_file (file: string) : Async<string * Result<bool, string>> =
    async {
        let! text = Async.AwaitTask(File.ReadAllTextAsync(file))
        let! formatted = format_source_code_async(text)

        if text <> formatted then
            do! Async.AwaitTask(File.WriteAllTextAsync(file, formatted))
            return true
        else
            return false
    }
    |> Async.Catch
    |> Async.map(
        function
        | Choice1Of2 needs_format -> file, Ok needs_format
        | Choice2Of2 err -> file, Error err.Message
    )

let walk_tree_specific_file (target: string) : string option =
    let mutable current_path = Path.GetFullPath(".")

    while current_path <> null && not(File.Exists(Path.Combine(current_path, target))) do
        current_path <- Path.GetDirectoryName(current_path)

    if current_path = null then None else Some(Path.Combine(current_path, target))

let get_ignore_list () : Ignore =
    match walk_tree_specific_file(".fantomasignore") with
    | Some ignore_file ->
        let lines = File.ReadAllLines(ignore_file)
        Array.fold<string, Ignore> _.Add (Ignore()) lines
    | None -> Ignore()

let get_files () : string array =
    let cwd = Directory.GetCurrentDirectory()
    let ignore = get_ignore_list()

    Directory.GetFiles(cwd, "*.fs", SearchOption.AllDirectories)
    |> Array.filter(fun path ->
        let relative = Path.GetRelativePath(cwd, path).Replace("\\", "/")

        not(ignore.IsIgnored(relative))
    )

[<Literal>]
let FSHARP_LINT_FILE_LIMIT = 25

let check_files_fsharplint (files: string array) : unit =
    let fsharplint_config = Configuration.parseConfig(Config.GetText("fsharplint.json"))

    let lint_config: OptionalLintParameters =
        {
            ReceivedWarning = None
            CancellationToken = None
            Configuration = Configuration(fsharplint_config)
            ReportLinterProgress = None
        }

    let lint_results = asyncLintFiles lint_config files |> Async.RunSynchronously

    match lint_results with
    | LintResult.Success warnings ->
        for w in warnings |> Seq.where(AllowShoutingSnakeCaseRule.filter_fsharplint_warning) do
            printfn
                "%s(%i,%i,%i,%i): %s: %s"
                w.FilePath
                w.Details.Range.StartLine
                w.Details.Range.StartColumn
                w.Details.Range.EndLine
                w.Details.Range.EndColumn
                w.RuleIdentifier
                w.Details.Message
    | LintResult.Failure reason -> printfn "DF0002: Error while linting! %s" reason.Description

let check_files_fantomas (files: string array) : unit =
    let check_results =
        files |> Array.map(check_file) |> Async.Parallel |> Async.RunSynchronously

    for result in check_results do
        match result with
        | Ok messages ->
            for message in messages do
                printfn "%O" message
        | Error(file, reason) -> printfn "%s: DF0000: Error while checking formatting! %s" file reason

let check_files () : unit =
    let files = get_files()

    if files.Length > FSHARP_LINT_FILE_LIMIT then
        printfn "Skipping FSharpLint checks as there are over %i files" FSHARP_LINT_FILE_LIMIT
    else
        check_files_fsharplint(files)

    check_files_fantomas(files)

let format_files () : unit =
    let files = get_files()

    let format_results =
        files |> Array.map(format_file) |> Async.Parallel |> Async.RunSynchronously

    let mutable formatted = 0
    let mutable unchanged = 0

    for file, result in format_results do
        match result with
        | Ok true -> formatted <- formatted + 1
        | Ok false -> unchanged <- unchanged + 1
        | Error reason -> printfn "%s: DF0000: Error while checking formatting! %s" file reason

    printfn "%i files formatted. %i files unchanged." formatted unchanged

let write_config () : unit =

    let inline get_config_directory () : string option =
        match walk_tree_specific_file(".fantomasignore") with
        | Some ignore_file -> Some(Path.GetDirectoryName(ignore_file))
        | None -> None

    match get_config_directory() with
    | None -> printfn "Could not detect where to write config!"
    | Some location ->
        let editorconfig_path = Path.Combine(location, ".editorconfig")
        let fantomasignore_path = Path.Combine(location, ".fantomasignore")

        File.WriteAllText(editorconfig_path, Config.GetText(".editorconfig"))

        if not(File.Exists(fantomasignore_path)) then
            File.WriteAllLines(fantomasignore_path, [| "**/bin"; "**/obj" |])

[<EntryPoint>]
let main (argv: string array) : int =
    let arg = if argv.Length > 0 then argv.[0] else ""

    match arg with
    | "check" -> check_files()
    | "format" -> format_files()
    | "init" -> write_config()
    | _ -> printfn "usage: defacto [check | format | init]"

    0
