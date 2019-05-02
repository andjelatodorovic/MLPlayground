namespace MachineLearning

open System
open System.IO

type ScrapRecord = 
    { SubId : string
      Features : float * float
      Scrap : string }

type ScrapData(fileName: string) = 

    do if String.IsNullOrEmpty(fileName) 
       then invalidArg "fileName" (sprintf "Invalid argument")

    member this.File with get() = Path.Combine(__SOURCE_DIRECTORY__, "Data", fileName)

    member this.Load = 
        
        let map (line: string) = 
            let values = line.Split([| '\t' |], StringSplitOptions.RemoveEmptyEntries)
            let subId = values.[0]
            let obs = float (values.[1]), float (values.[2])
            let label = values.[3]
    
            let scrap = 
                { SubId = subId
                  Features = obs
                  Scrap = label }
            scrap
        
        File.ReadAllLines(this.File).[1..] 
        |> Array.map(fun line -> line |> map)

