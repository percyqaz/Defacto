namespace Defacto

open System.Text.RegularExpressions

module MembersMissingTypeAnnotationRule =

    let regex =
        Regex(
            "(member|override)(?>( internal| private)?)(?>( inline)?)( this\.)?[A-Za-z]+\([^\)]*\) =",
            RegexOptions.Compiled
        )

    let find_matches (file_path: string, source_text: string) : Message seq =

        let index_to_line_number (index: int) : int * int =
            let rec loop (current_line: int) (current_index: int) =
                let next_index = source_text.IndexOf('\n', current_index)

                if next_index > index || next_index < 0 then
                    current_line, (index - current_index) + 1
                else
                    loop (current_line + 1) (next_index + 1)

            loop 1 0

        seq {
            for regex_match in regex.Matches(source_text) do
                let line_n, line_pos = index_to_line_number(regex_match.Index + 1)
                yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = DF0003 }
        }
