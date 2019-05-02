namespace MachineLearning

#load "LogisticRegression.fsx"
#load "ScrapData.fsx"

open System

// Utility Types & Functions
module Utility = 

    //(for Scrap Data Normalize CdsThickness) -- i think this translates to Z-Score Normalization
    
    // Calculate Mean (x1 + x2+ x3+..) / N
    let calculateMean  (scrapRecords: MachineLearning.ScrapRecord[]) = 
        (scrapRecords 
            |> Array.sumBy (fun scrap -> snd scrap.Features)) / float (scrapRecords.Length)
    
    // Calculate Standard Dev.
    let calculateStdDev mean (scrapRecords: MachineLearning.ScrapRecord[]) = 
        sqrt (scrapRecords
                |> Array.map (fun scrap -> (snd scrap.Features - mean) ** 2.0)
                |> Array.sum) / float (scrapRecords.Length)
    
    // prepare data 
    let generateData mean stdDev (scrapRecords: MachineLearning.ScrapRecord[])  = 
        scrapRecords 
            |> Array.map (fun record -> 
                            let features = [| 1. ; fst record.Features ; abs((snd record.Features - mean) / stdDev) |]
                            let label = match record.Scrap with
                                        | "Yes" -> 1.
                                        | "No"  -> 0.
                                        |  _    -> -1.
                                    
                            features, label)

module Train =
    
    open Utility
    open MachineLearning

    // Load Training Data
    let scrap = new ScrapData("TrainingData.txt")
    let scrapRecords = scrap.Load
    
    //Normalize and prepare data for Training
    let mean = scrapRecords |> calculateMean 
    let stdDev = scrapRecords |> calculateStdDev mean

    let trainData = scrapRecords |> generateData mean stdDev        

    let model = new LogisticRegression()
    let weights = model.TrainData(trainData, 3, 1000, 0.001, 0)

    let accuracy = trainData |> model.CheckAccuracy weights   

module Validate = 
    
    open Utility
    open MachineLearning

    // Load Validation Data
    let scrap = new ScrapData("ValidationData.txt")
    let scrapRecords = scrap.Load

    //Normalize and prepare data for Validation
    let mean = scrapRecords |> calculateMean 
    let stdDev = scrapRecords |> calculateStdDev mean

    let validationData = scrapRecords |> generateData mean stdDev        

    let model = new LogisticRegression()
    let weights = [|-0.8712211052; 1.807320567; -0.4008330772|] 
    let accuracy = validationData |> model.CheckAccuracy weights


#load @"..\Packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"
#load @"..\Packages\MathNet.Numerics.FSharp.3.11.1\MathNet.Numerics.fsx"

module ModelMathNet = 
    
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearRegression
    open FSharp.Charting        
    open Utility 
    open MachineLearning   

    // Load Training Data
    let scrap = new ScrapData("TrainingData.txt")
    let scrapRecords = scrap.Load

    // Normalize and prepare data for Training
    let mean = scrapRecords |> calculateMean 
    let stdDev = scrapRecords |> calculateStdDev mean

    let normalizedData = scrapRecords 
                            |> generateData mean stdDev

    let trainData = normalizedData
                    |> Array.map(fun record -> (fst record).[1..])
                         
                    
    let labelData = normalizedData 
                    |> Array.map(fun record -> snd record)

    // Setup Matrices 
    let X = DenseMatrix.ofRowArrays trainData                           // Features
    let rows = X.RowCount                                               // Number of Records
    let Y = DenseMatrix.initRows rows (fun i -> vector[labelData.[i]])  // Labels
    let theta = DenseMatrix.create rows 1 0.                            // Weights
    
    // Helper variables & functions
    let alpha = 0.25
    let sigmoid x = 1. / (1. + exp -x) 

    // Chart Sigmoid
    let chartValues = [-20. .. 20.] 
                        |> List.map(fun i -> sigmoid i) 
                        |> Chart.Line 

    // the First Equation Matrix 
    let h = theta.Transpose() * X |> Matrix.map(fun t -> sigmoid t)

    // The intermediate equation
    let thetaleft = theta  - (alpha / (float rows))
    let thetaRight = X * h.Transpose() - Y

    // The Final REsult
    let thetaM = thetaleft.Transpose() * thetaRight


    // Predict
    let unknownX = matrix[[2.20181069; 4.88239006]]
    let predictionLR = thetaM.Transpose()  * unknownX

    let pR = log10 (abs(predictionLR.At(0, 0)))
