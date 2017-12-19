let GetWordStats(s:string) = s |> Seq.countBy(fun index -> index) |> Seq.sortBy fst |> Seq.toList

let AnagramDistinct(words:string[]) =
    let mutable UniqueWords = []
    for i in words do
        let letters = i |> GetWordStats
        //Fix this later
        let size = words  |> Array.where(fun elem -> (elem |> GetWordStats |> List.ofSeq) = List.ofSeq letters)
        printfn "%A" <| List.ofSeq letters 
        UniqueWords <- if size.Length > 1 then UniqueWords else UniqueWords |> List.append [i]
    UniqueWords


let UniquePass(s:string) =
    let mutable counter = 0
    let words = s.Split('\n')
    for i in words do
        let cleanPass = i.Remove(i.Length - 1, 1)
        let pass = cleanPass.Split(' ')
        let uniquePass = pass |> AnagramDistinct
        counter <- if uniquePass.Length = pass.Length then counter + 1 else counter
    counter





[<EntryPoint>]
let main argv = 
    printfn "%A" <| UniquePass(System.IO.File.ReadAllText("input.txt"))
    0 // return an integer exit code
