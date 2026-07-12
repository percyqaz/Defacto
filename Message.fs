namespace Defacto

type MessageId =
    | DF0001
    | DF0003
    | DF0004

    override this.ToString() : string = sprintf "%A: %s" this this.Message

    member this.Message: string =
        match this with
        | DF0001 -> "File needs formatting."
        | DF0003 -> "Method missing return type annotation."
        | DF0004 -> "Banned symbol (likely to cause mistakes or confusion)."

type Message =
    {
        FilePath: string
        Location: (int * int) option
        Id: MessageId
    }

    override this.ToString() : string =
        match this.Location with
        | Some(line, column) -> sprintf "%s(%i,%i): %O" this.FilePath line column this.Id
        | None -> sprintf "%s: %O" this.FilePath this.Id

    static member IndexToLocation(source_text: string, index: int) : int * int =
        let rec loop (current_line: int) (current_index: int) =
            let next_index = source_text.IndexOf('\n', current_index)

            if next_index > index || next_index < 0 then
                current_line, (index - current_index) + 1
            else
                loop (current_line + 1) (next_index + 1)

        loop 1 0
