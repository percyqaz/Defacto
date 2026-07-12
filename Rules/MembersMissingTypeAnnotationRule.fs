namespace Defacto

open System.Text.RegularExpressions

module MembersMissingTypeAnnotationRule =

    let regex =
        Regex(
            "(member|override)(?>( internal| private)?)(?>( inline)?)( this\.)?[A-Za-z]+\([^\)]*\) =",
            RegexOptions.Compiled
        )

    let find_matches (file_path: string, source_text: string) : Message seq =

        seq {
            for regex_match in regex.Matches(source_text) do
                let line_n, line_pos = Message.IndexToLocation(source_text, regex_match.Index + 1)
                yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = DF0003 }
        }
