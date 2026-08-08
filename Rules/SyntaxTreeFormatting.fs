namespace Defacto

open System
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text

module SyntaxTreeFormatting =

    type Fix = { Position: Position; Insert: string }

    let private find_fixes (oak: Oak) : ResizeArray<Fix> =
        let fixes = ResizeArray<Fix>()

        let inline missing_indexing_dot (expr: ExprIndexWithoutDotNode) : Fix seq =
            [ { Position = expr.Children.[0].Range.End; Insert = "." } ]

        let inline pascal_case_method_applied_to_single_arg (expr: ExprAppNode) : Fix seq =
            match expr.FunctionExpr with
            | Expr.OptVar v when List.tryExactlyOne(expr.Arguments).IsSome ->
                match List.last(v.Identifier.Content) with
                | IdentifierOrDot.Ident i ->
                    if Char.IsUpper(i.Text.[0]) then
                        [
                            { Position = i.Range.End; Insert = "(" }
                            { Position = expr.Range.End; Insert = ")" }
                        ]
                    else
                        []
                | _ -> []
            | _ -> []

        let rec walk_all_nodes (node: Node) =
            match node with
            | :? ExprIndexWithoutDotNode as expr -> fixes.AddRange(missing_indexing_dot(expr))
            | :? ExprAppNode as expr -> fixes.AddRange(pascal_case_method_applied_to_single_arg(expr))
            | _ -> ()

            for child in node.Children do
                walk_all_nodes(child)

        for child in oak.Children do
            walk_all_nodes(child)

        fixes

    let find_and_apply_fixes (source_text: string) : Async<string> =
        async {
            let source_lines = source_text.Split("\n")

            let! ast_array = CodeFormatter.ParseOakAsync(false, source_text)
            let ast, _ = ast_array.[0]

            let fixes_bottom_to_top =
                find_fixes(ast) |> Seq.sortByDescending(fun x -> x.Position.Line, x.Position.Column)

            for fix in fixes_bottom_to_top do
                source_lines.[fix.Position.Line - 1] <-
                    source_lines.[fix.Position.Line - 1].Insert(fix.Position.Column, fix.Insert)

            return String.concat "\n" source_lines
        }
