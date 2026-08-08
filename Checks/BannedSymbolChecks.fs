namespace Defacto

module BannedSymbolChecks =

    let BANNED_SYMBOLS =
        Config.GetText("BannedSymbols.txt").Replace("\r", "").Split("\n")

    let find_matches (file_path: string, source_text: string) : Message seq =

        seq {
            for symbol in BANNED_SYMBOLS do
                let mutable index = source_text.IndexOf(symbol)

                while index >= 0 do
                    yield
                        {
                            Id = DF0004
                            FilePath = file_path
                            Location = Some(Message.IndexToLocation(source_text, index))
                        }

                    index <- source_text.IndexOf(symbol, index + symbol.Length)
        }
