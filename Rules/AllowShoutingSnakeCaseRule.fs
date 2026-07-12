namespace Defacto

open FSharpLint.Framework

module AllowShoutingSnakeCaseRule =

    let filter_fsharplint_warning (warning: Suggestion.LintWarning) : bool =
        if warning.RuleIdentifier = "FL0049" then
            let identifier = warning.Details.Message.Split("`").[1]
            identifier.ToUpper() <> identifier
        else
            true
