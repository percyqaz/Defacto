open System.IO
open Fantomas.Core
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
// todo: auto-write editorconfig and fsharplint.json
// todo: auto-add .editorconfig and fsharplint.json to .gitignore if exists

let source_text =
    "namespace Defacto.Test\nopen System.IO\nmodule Test = let hello = System.Console.WriteLine 2; List.concat [3;2] [Console.read 4]"

let format_source_code_async (source_text: string) : Async<string> =
    async {
        let! source_text_fixed = PascalCaseSingleArgumentBracketsRule.fix_brackets_source_text_async(source_text)

        let! ast_array = CodeFormatter.ParseOakAsync(false, source_text_fixed)
        let ast, _ = ast_array.[0]
        return! CodeFormatter.FormatOakAsync(ast, config)
    }

let check_file (file: string) : Async<string * Result<bool, string>> =
    async {
        let! text = Async.AwaitTask(File.ReadAllTextAsync(file))
        let! formatted = format_source_code_async(text)
        return (text <> formatted)
    }
    |> Async.Catch
    |> Async.map(
        function
        | Choice1Of2 needs_format -> file, Ok needs_format
        | Choice2Of2 err -> file, Error err.Message
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

open Ignore

[<EntryPoint>]
let main argv : int =

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

    let get_files () =
        let cwd = Directory.GetCurrentDirectory()
        let ignore = get_ignore_list()

        Directory.GetFiles(cwd, "*.fs", SearchOption.AllDirectories)
        |> Array.filter(fun path ->
            let relative = Path.GetRelativePath(cwd, path).Replace("\\", "/")

            not(ignore.IsIgnored(relative))
        )

    let check_files () =
        let files = get_files()

        let check_results =
            files |> Array.map check_file |> Async.Parallel |> Async.RunSynchronously

        for file, result in check_results do
            match result with
            | Ok true -> printfn "%s: DF0001: Needs formatting" file
            | Ok false -> ()
            | Error reason -> printfn "%s: DF0000: Error while checking formatting! %s" file reason

    let format_files () =
        let files = get_files()

        let format_results =
            files |> Array.map format_file |> Async.Parallel |> Async.RunSynchronously

        let mutable formatted = 0
        let mutable unchanged = 0

        for file, result in format_results do
            match result with
            | Ok true -> formatted <- formatted + 1
            | Ok false -> unchanged <- unchanged + 1
            | Error reason -> printfn "%s: DF0000: Error while checking formatting! %s" file reason

        printfn "%i files formatted. %i files unchanged." formatted unchanged

    let arg = if argv.Length > 0 then argv.[0] else ""

    match arg with
    | "check" -> check_files()
    | "format" -> format_files()
    | _ -> printfn "usage: defacto check, defacto format"

    0
