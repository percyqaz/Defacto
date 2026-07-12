namespace Defacto

open System.Reflection
open System.IO

type Config =

    static member GetStream(name: string) : Stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Defacto.Config." + name)

    static member GetText(name: string) : string =
        use s = Config.GetStream(name)
        use tr = new StreamReader(s)
        tr.ReadToEnd()
