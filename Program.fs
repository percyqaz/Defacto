open System.IO
open Fantomas.Core
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

let check_file (file: string) : Async<Result<Message list, string * string>> =
    async {
        let messages = ResizeArray<Message>()

        let! source_text = Async.AwaitTask(File.ReadAllTextAsync(file))
        let! source_text_fixed = SyntaxTreeFormatting.find_and_apply_fixes(source_text)
        let! ast_array = CodeFormatter.ParseOakAsync(false, source_text_fixed)
        let! formatted = CodeFormatter.FormatOakAsync(fst ast_array.[0], config)

        if source_text <> formatted then
            messages.Add({ Id = DF0001; FilePath = file; Location = None })

        messages.AddRange(SyntaxTreeChecks.find_warnings(fst ast_array.[0]) |> Seq.map _.ToMessage(file))
        messages.AddRange(RegexCheckWarnings.find_matches(file, source_text))
        messages.AddRange(BannedSymbolCheckWarnings.find_matches(file, source_text))

        return messages |> Seq.sortBy _.Location |> List.ofSeq
    }
    |> Async.Catch
    |> Async.map(
        function
        | Choice1Of2 messages -> Ok messages
        | Choice2Of2 err -> Error(file, err.Message)
    )

let format_file (file: string) : Async<string * Result<bool, string>> =

    let format_source_code_async (source_text: string) : Async<string> =
        async {
            let! source_text_fixed = SyntaxTreeFormatting.find_and_apply_fixes(source_text)
            let! ast_array = CodeFormatter.ParseOakAsync(false, source_text_fixed)
            return! CodeFormatter.FormatOakAsync(fst ast_array.[0], config)
        }

    async {
        let! source_text = Async.AwaitTask(File.ReadAllTextAsync(file))
        let! formatted = format_source_code_async(source_text)

        if source_text <> formatted then
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

let check_files () : Result<unit, unit> =
    let files = get_files()

    let check_results =
        files |> Array.map(check_file) |> Async.Parallel |> Async.RunSynchronously

    let mutable any_warnings = false

    for result in check_results do
        match result with
        | Ok messages ->
            for message in messages do
                printfn "%O" message
                any_warnings <- true
        | Error(file, reason) ->
            printfn "%s: DF0000: Error while checking formatting! %s" file reason
            any_warnings <- true

    if any_warnings then Error() else Ok()

let format_files () : Result<unit, unit> =
    let files = get_files()

    let format_results =
        files |> Array.map(format_file) |> Async.Parallel |> Async.RunSynchronously

    let mutable formatted = 0
    let mutable unchanged = 0
    let mutable errors = 0

    for file, result in format_results do
        match result with
        | Ok true -> formatted <- formatted + 1
        | Ok false -> unchanged <- unchanged + 1
        | Error reason ->
            printfn "%s: DF0000: Error while formatting! %s" file reason
            errors <- errors + 1

    printfn "%i files formatted. %i files unchanged." formatted unchanged
    if errors <> 0 then Error() else Ok()

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
    | "check" ->
        match check_files() with
        | Ok() -> 0
        | Error() -> 1
    | "format" ->
        match format_files() with
        | Ok() -> 0
        | Error() -> 1
    | "init" ->
        write_config()
        0
    | _ ->
        printfn "usage: defacto [check | format | init]"
        0
