namespace Defacto

module BannedSymbolsRule =

    let BANNED_SYMBOLS =
        Config.GetText("BannedSymbols.txt").Replace("\r", "").Split("\n")

    let find_matches (file_path: string, source_text: string) : Message seq =

        seq {
            for symbol in BANNED_SYMBOLS do
                let index = source_text.IndexOf(symbol)

                if index >= 0 then
                    yield
                        {
                            Id = DF0004
                            FilePath = file_path
                            Location = Some(Message.IndexToLocation(source_text, index))
                        }
        }
