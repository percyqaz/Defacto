namespace Defacto

type MessageId =
    | DF0001
    | DF0002
    | DF0003
    | DF0004
    | DF0005
    | DF0006

    member this.Message: string =
        match this with
        | DF0001 -> "File needs formatting."
        | DF0002 -> "Local variables and parameters should be snake_case"
        | DF0003 -> "Method requires return type annotation"
        | DF0004 -> "Banned symbol (likely to cause mistakes or confusion)"
        | DF0005 -> "Member names should be PascalCase"
        | DF0006 -> "Non-static methods should be prefixed with 'this.'"

type Message =
    {
        FilePath: string
        Location: (int * int) option
        Id: MessageId
        Message: string
    }

    override this.ToString() : string =
        match this.Location with
        | Some(line, column) -> sprintf "%s(%i,%i): %O: %s" this.FilePath line column this.Id this.Message
        | None -> sprintf "%s: %O: %s" this.FilePath this.Id this.Message

    static member IndexToLocation(source_text: string, index: int) : int * int =
        let rec loop (current_line: int) (current_index: int) =
            let next_index = source_text.IndexOf('\n', current_index)

            if next_index > index || next_index < 0 then
                current_line, (index - current_index) + 1
            else
                loop (current_line + 1) (next_index + 1)

        loop 1 0
