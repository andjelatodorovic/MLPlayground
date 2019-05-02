namespace MachineLearning

// logistic [ x1; x2; … xn ] = 1.0 / (1.0 + exp ( - (w0 * 1.0 + w1 * x1 + w2 * x2 + … + wn * xn))
type LogisticRegression() = 

    let sumprod (v1: float[]) (v2: float[]) = 
        Seq.zip v1 v2
        |> Seq.sumBy(fun (x , y) -> x * y)
    
    let sigmoid x = 1.0 / (1.0 + exp -x)

    let logistic (features: float[]) (weights: float[]) =
        sumprod features weights 
        |> sigmoid

    let error (data: (float [] * float) [], weights: float[]) = 
        // mean squared error / deviation
        data 
        |> Array.averageBy(fun (record) ->
            let computed = logistic (fst record) weights
            let desired = snd record 
            (computed - desired) * (computed - desired))

    member this.TrainData ((data : (float [] * float) [] ), numfeatures, maxEpochs, alpha, seed) = 
        let epoch = 0

        let update (data: float [] * float) (weights: float[]) = 
            let features = fst data
            let label = snd data
            let computed = logistic features weights            
            weights
            |> Array.mapi(fun i w -> w + alpha * (label - computed) * features.[i])

        let rec updateWeights (data: (float [] * float) []) epoch weights = 
        
            if epoch % 100 = 0
            then printfn "Epoch: %i, Error %.2f" epoch (error (data,weights))

            if epoch = maxEpochs then weights
            else
                // Update weights
                let weights = data |> Array.fold(fun weights dataRecord -> update dataRecord weights) weights
                updateWeights data (epoch + 1) weights
    
        // Initialize the weights and start the update 
        let initialWeights = [| for _ in 1 .. numfeatures -> 0. |]
        let finalWeights = updateWeights data 0 initialWeights

        finalWeights

    member this.Classify weights (data: (float [] * float)) = 
        if logistic (fst data) weights > 0.5 then 1. else 0.

    member this.CheckAccuracy weights (data: (float [] * float)[]) =
        data
        |> Array.averageBy(fun (record) ->
                let label = snd record
                if this.Classify weights record = label then 1. else 0.)