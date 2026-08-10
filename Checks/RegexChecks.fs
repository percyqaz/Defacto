namespace Defacto

open System.Text.RegularExpressions

module RegexChecks =

    let members_always_use_this =
        Regex(
            @"(member|override)(?>( val)?)(?>( internal| private)?)(?>( inline)?) (?!this)[A-Za-z0-9]+\.",
            RegexOptions.Compiled
        )

    let members_require_type_annotation =
        Regex(
            @"(static )?(member|override)(?>( val)?)(?>( internal| private)?)(?>( inline)?) (?>(this\.)?)[A-Za-z0-9]+\s*\([^\)]*\) =",
            RegexOptions.Compiled
        )

    let members_pascal_case =
        Regex(
            @"(static )?(member|override)(?>( val)?)(?>( internal| private)?)(?>( inline)?) (?>(this\.)?)(?![A-Z][A-Za-z0-9]*[\s\(:]|\()",
            RegexOptions.Compiled
        )

    let find_matches (file_path: string, source_text: string) : Message seq =
        seq {
            for m in members_require_type_annotation.Matches(source_text) do
                let line_n, line_pos = Message.IndexToLocation(source_text, m.Index + 1)
                yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = DF0003 }

            for m in members_pascal_case.Matches(source_text) do
                let line_n, line_pos = Message.IndexToLocation(source_text, m.Index)
                yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = DF0005 }

            for m in members_always_use_this.Matches(source_text) do
                let line_n, line_pos = Message.IndexToLocation(source_text, m.Index)
                yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = DF0006 }
        }
