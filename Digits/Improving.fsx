open System
open System.IO

type Example = { Label:int; Pixels:int[] }

let dropHeaders (data:string[]) = data.[1..]

let readData (path:string) =
    File.ReadAllLines(path)
    |> dropHeaders
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun line -> line |> Array.map int)
    |> Array.map (fun line -> { Label = line.[0]; Pixels = line.[1..] })

let trainingPath =  __SOURCE_DIRECTORY__ + @"\trainingsample.csv"
let validationPath = __SOURCE_DIRECTORY__ + @"\validationsample.csv"

let training = readData trainingPath
let validation = readData validationPath 

type Pixels = int []

let redNoiseLow x = if x < 30 then 0 else x
let redNoiseHigh x = if x > 220 then 255 else x
let redNoise x = redNoiseLow (redNoiseHigh x)

let diff x1 x2 = x1 - x2
//let diff x1 x2 = (redNoise x1) - (redNoise x2)

let distance (pix1:Pixels) (pix2:Pixels) =
    (pix1, pix2) 
    ||> Array.map2 (fun x1 x2 -> pown (diff x1 x2) 2)
    |> Array.sum

let classify (pix:Pixels) =
    training
    |> Array.minBy (fun ex -> distance pix ex.Pixels)
    |> fun ex -> ex.Label //if (ex.Label = 4) then 9 else ex.Label

//
//    
//let majority (candidates:int[])
//    
//let classify (pix:Pixels) =
//    training
//    |> Array.map (fun ex -> distance pix ex.Pixels, ex.Label)
//    |> Array.sort
//    |> fun x -> x.[0..2]
//    |> fun ex -> fst ex // ex.Label //if (ex.Label = 4) then 9 else ex.Label


let quality (sample:Example[]) =
    sample
//    |> Array.map (fun ex -> if(classify(ex.Pixels) = ex.Label) then 100,100 else classify(ex.Pixels),ex.Label)
    |> Array.map (fun ex -> 
        if classify (ex.Pixels) = ex.Label then 1. else 0.)
    |> Array.average
 
printfn "Quality: %.4f" (quality validation)

(*
Improving the model: multiple possibilities

Improving speed 

Check Array.Parallel, and #time

Improving quality: some possible directions

1. Use a different distance
   ex: d = abs(x1-y1) + ... abs(xk-yk)
   ex: d = abs(x1-y1)^3 + ...

2. Instead of 1 neighbor, use k neighbors 
and take a majority vote. What is the best k? 

3. "reduce noise": instead of 0 to 255, reduce
pixels encoding to pix/2, pix/3, ...
What is the best value?

4. "reduce noise": instead of comparing individual
pixels, compare square blocks of pixels: 2x2, 3x3, ...
What is the best value?


*)

let size = 28
let reduce (pix:Pixels) (width:int) =
    [| 
        for row in 0 .. (size - width) do
            for col in 0 .. (size - width) do
                let total =
                    [
                        for r in 0 .. (width - 1) do
                            for c in 0 .. (width - 1) do
                                yield pix.[(row + r) * size + col + c]
                    ]
                    |> List.sum
                    |> fun x -> x / width
                yield total
    |]