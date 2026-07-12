namespace Defacto

type MessageId =
    | DF0001
    | DF0003
    override this.ToString() : string =
        sprintf "%A: %s" this this.Message
    member this.Message : string =
        match this with
        | DF0001 -> "File needs formatting."
        | DF0003 -> "Method missing return type annotation."

type Message =
    { FilePath: string; Location: (int * int) option; Id: MessageId }
    override this.ToString() : string =
        match this.Location with
        | Some (line, column) -> sprintf "%s(%i,%i): %O" this.FilePath line column this.Id
        | None -> sprintf "%s: %O" this.FilePath this.Id