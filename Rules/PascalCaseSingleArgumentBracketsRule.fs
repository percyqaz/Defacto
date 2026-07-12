namespace Defacto

open System
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text

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