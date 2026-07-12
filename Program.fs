open System
open System.IO
open Fantomas.Core

open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text

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

module PascalCaseSingleArgumentBracketsRule =

    let find_missing_brackets_oak (oak: Oak) : ResizeArray<pos * pos> =
        let ranges = ResizeArray<pos * pos>()

        let inline detect_pascal_case_method_applied_to_single_arg (expr: ExprAppNode) =
            match expr.FunctionExpr with
            | Expr.OptVar v when List.tryExactlyOne(expr.Arguments).IsSome ->
                match List.last(v.Identifier.Content) with
                | IdentifierOrDot.Ident i ->
                    if Char.IsUpper(i.Text.[0]) then
                        ranges.Add(i.Range.End, expr.Range.End)
                | _ -> ()
            | _ -> ()

        let rec walk_all_applications (n: Node) =
            match n with
            | :? ExprAppNode as expr -> detect_pascal_case_method_applied_to_single_arg(expr)
            | _ -> ()

            for c in n.Children do
                walk_all_applications(c)

        for c in oak.Children do
            walk_all_applications(c)

        ranges

    let fix_brackets_source_text_async (source_text: string) : Async<string> =
        async {
            let source_lines = source_text.Split("\n")

            let! ast_array = CodeFormatter.ParseOakAsync(false, source_text)
            let ast, _ = ast_array.[0]

            for a, b in Seq.rev(find_missing_brackets_oak ast) do
                source_lines.[b.Line - 1] <- source_lines.[b.Line - 1].Insert(b.Column, ")")
                source_lines.[a.Line - 1] <- source_lines.[a.Line - 1].Insert(a.Column, "(")

            return String.concat "\n" source_lines
        }

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

[<EntryPoint>]
let main argv : int =

    let get_files () =
        let cwd = Directory.GetCurrentDirectory()
        let ignore = Ignore.Ignore().Add("**/bin").Add("**/obj")

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
