
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


let Day8(s:string[]) =
    let mutable register = Map.empty
    let mutable largestVal = 0
    //Each line
    for i in s do
        let instruction = i.Split(' ')
        //Value of the condition
        let conditionNum = (if register.ContainsKey instruction.[4] then register.[instruction.[4]] else 0) |> int
        let comparedNum = instruction.[6] |> int

        let result = match instruction.[5] with
                        | "==" -> conditionNum = comparedNum
                        | ">=" -> conditionNum >= comparedNum
                        | ">" -> conditionNum > comparedNum
                        | "<=" -> conditionNum <= comparedNum
                        | "<" -> conditionNum < comparedNum
                        | "!=" -> conditionNum <> comparedNum
                        | _ -> failwith "Unexpected comparison operator"
        
        //Does the value to update exist? If yes thats fine, if not, create it and have it at 0
        register <- if register.ContainsKey instruction.[0] then register else register.Add(instruction.[0], 0)

        let update = match instruction.[1] with
                        | "inc" -> instruction.[2] |> int
                        | "dec" -> (-1)*(instruction.[2] |> int)
                        | _ -> failwith "Unexpected command received"

        register <- if result then register.Add(instruction.[0], register.[instruction.[0]] + update)
                    else register
        let currentLargest = (register |> Seq.maxBy(fun elem -> elem.Value)).Value
        largestVal <- if largestVal < currentLargest then currentLargest else largestVal
    ((register |> Seq.maxBy(fun elem -> elem.Value)).Value, largestVal)
    

[<EntryPoint>]
let main argv = 
    printfn "%A" <| Day8(System.IO.File.ReadAllLines("input.txt"))
    0 // return an integer exit code

