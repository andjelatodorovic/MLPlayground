namespace MachineLearning

/// <summary>
/// ScrapPrediction Class to predict Scrap
/// </summary>
type ScrapPrediction() = 
            
    static let weights = [| -0.8712211052; 1.807320567; -0.4008330772 |]
    
    static let sumprod (v1: float[]) (v2: float[]) = 
        Seq.zip v1 v2
        |> Seq.sumBy(fun (x , y) -> x * y)

    static let sigmoid x = 1.0 / (1.0 + exp -x)

    static let logistic (features: float[]) (weights: float[]) =
        sumprod features weights 
        |> sigmoid
    
    /// <summary>
    /// IsScrap Method to return Tuple of float,bool
    /// </summary>
    /// <param name="features"></param>
    static member IsScrap (features : float[]) = 
        let probability = logistic features weights
        
        if probability > 0.5 then
            probability, true
        else
            probability, false
        