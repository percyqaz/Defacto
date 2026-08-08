namespace Defacto

open System
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text

module SyntaxTreeChecks =

    type Warning =
        {
            Position: Position
            Id: MessageId
        }

        member this.ToMessage(file: string) : Message =
            { FilePath = file; Id = this.Id; Location = Some(this.Position.Line, this.Position.Column) }

    let find_warnings (oak: Oak) : ResizeArray<Warning> =
        let warnings = ResizeArray<Warning>()

        let inline is_snake_case (name: string) : bool =
            not(name.Contains("__")) && String.forall (Char.IsUpper >> not) name

        let inline is_shouting_snake_case (name: string) : bool =
            not(name.Contains("__")) && String.forall (Char.IsLower >> not) name

        let inline snake_case_let_ident_list_node (x: IdentListNode, allow_shouting: bool) : Warning seq =
            match List.head x.Content with
            | IdentifierOrDot.Ident x ->
                if is_snake_case(x.Text) then []
                elif allow_shouting && is_shouting_snake_case(x.Text) then []
                else [ { Position = x.Range.Start; Id = DF0002 } ]
            | _ -> []

        let rec snake_case_let_pattern (x: Pattern) : Warning seq =
            match x with
            | Pattern.StructTuple t -> Seq.collect snake_case_let_pattern t.Patterns
            | Pattern.Tuple t ->
                seq {
                    for item in t.Items do
                        match item with
                        | Choice1Of2 inner_pattern -> yield! snake_case_let_pattern(inner_pattern)
                        | Choice2Of2 text ->
                            if not(is_snake_case(text.Text)) then
                                yield { Position = text.Range.Start; Id = DF0002 }
                }
            | Pattern.Wild _ -> []
            | Pattern.Named n ->
                if is_snake_case(n.Name.Text) then [] else [ { Position = n.Name.Range.Start; Id = DF0002 } ]
            | Pattern.LongIdent n -> snake_case_let_ident_list_node(n.Identifier, false)
            | _ -> [] // future: could support other patterns like record decons, list decons, etc

        let inline snake_case_let (node: ExprLetOrUseNode) : Warning seq =
            match node.Binding.FunctionName with
            | Choice1Of2 ident_list -> snake_case_let_ident_list_node(ident_list, List.isEmpty node.Binding.Parameters)
            | Choice2Of2 pat -> snake_case_let_pattern(pat)

        let rec walk_all_nodes (node: Node) =
            match node with
            | :? ExprLetOrUseNode as expr -> warnings.AddRange(snake_case_let(expr))
            | :? ExprLetOrUseBangNode as expr -> warnings.AddRange(snake_case_let_pattern(expr.Pattern))
            | _ -> ()

            for child in node.Children do
                walk_all_nodes(child)

        for child in oak.Children do
            walk_all_nodes(child)

        warnings
