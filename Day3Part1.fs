// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec FindSquareLength(x:int, num:int) = if x*x >= num then x  else FindSquareLength(x+2, num)


let position(num:int) =
    //Now we know the square's diameter
    let dim = FindSquareLength(1, num)

    //Biggest value of the square will always be (2k+1)^2 where k = 0,1,2,...,n. This is the bottom right edge of the square
    let upperValue = dim*dim

    //How far we are from our upperValue of our square
    let difference = upperValue - num

    //Coordinates of upperValue (1,9,25,49...)
    let mutable x = (dim - 1)/2
    let mutable y = -(dim - 1)/2
    
    //This is the ratio between the difference and the square's length. Will always have bounds [0,4)
    let steps = (difference |> double)/((dim |> double)-1.0)

    //Now we will iterate from 1 to the amount of steps to find the next corner we will be at. Note, we do not need to include the case 0
    //because the loop will just not run if we have 1..0
    for i in 1..steps |> floor |> int do 
        //Num has to be between 0 and 4
        match i with
        //Left
        | 1 -> x <- x - (dim - 1)
        //Right
        | 2 -> y <- y + (dim - 1)
        //Up
        | 3 -> x <- x + (dim - 1)
        //Down
        | 4 -> y <- y - (dim - 1)

    //Now that we are at the next closest edge of the square, we're left with the small remaining fraction of movement.
    let lastMovement = steps - (steps |> floor)

    match steps |> floor |> int with
        //That means that the closest edge is the same as upperValue, we move left
        | 0 -> x <- x - (int)(lastMovement*((double)(dim-1)))
        //We move up                        
        | 1 -> x <- y + (int)(lastMovement*((double)(dim-1)))
        //We move right                     
        | 2 -> x <- x + (int)(lastMovement*((double)(dim-1)))
        //We move down                      
        | 3 -> x <- y - (int)(lastMovement*((double)(dim-1)))
        //This is in case steps is NaN, which would only happen if we pick the square 1
        | _ ->
            x <- 0
            y <- 0

    //Final Value
    (x,y)
  
let ManhattanDistance(x:int, y:int) = abs(x) + abs(y)


[<EntryPoint>]
let main argv = 
    let pos = position 277678
    let solution = ManhattanDistance pos
    printfn "%A" solution
    0 // return an integer exit code
