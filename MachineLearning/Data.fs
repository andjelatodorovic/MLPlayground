namespace MachineLearning
   
open System

module Data =
    let load path filename =
        try
            let lines = System.IO.File.ReadAllLines(IO.Path.Combine(path, filename))
                        |> Seq.map (fun s -> s.Split [|','|] |> Seq.map Double.Parse |> Seq.toList)
                        |> Seq.toList
            lines
        with
            | :? System.IO.FileNotFoundException 
                -> printfn @"Could not find file"
                   Console.ReadKey() |> ignore
                   exit 0
            
    
    /// <summary>s
    /// This function converts a list of length two [x;y] to a tuple (x,y)
    /// </summary>
    /// <param name="list">A list of floats of length two.</param>
    /// <returns>A tuple (x,y)</returns>
    let private listToTuple list =
        match list with
        | [x; y] -> (x,y)
        | _ -> failwith "List must be of [x,y]"

    /// <summary>
    /// This function converts a list of a list of floats and converts it to a list of tuples of floats
    /// </summary>
    /// <param name="list">A list a list of floats</param>
    /// <returns>A list of tuples [(x1,y1);(x2,y2)..(xn,yn)]</returns>
    let toChartTuple list =
        list
            |> List.map listToTuple