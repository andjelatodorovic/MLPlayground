// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

namespace MachineLearning

open FSharp.Charting
open System
open System.Drawing
open System.Windows
open MathNet.Numerics.LinearAlgebra
open MachineLearning.Data
open MachineLearning.LinearRegression
open MachineLearning.LogisticRegression
open MachineLearning.Misc
open MachineLearning.Activation

module program =

    [<EntryPoint>]
    let main argv = 
        
        // Linear Regression
        let loadWithPath = load @"C:\Users\lukec\OneDrive\Documents\Visual Studio 2015\Projects\MachineLearning"

        (*
        let data = loadWithPath "ex1data1.txt"

        let matrixData = data |> matrix
        let chartData = data |> toChartTuple

        let y = matrixData.Column(1)
        let m = y.Count
        let X = addOnesColumn <| matrixData.Column(0).ToColumnMatrix()
        let theta = Vector<double>.Build.Dense(X.ColumnCount)

        let alpha = 0.0003
        let num_iters = 100

        let J = LinearRegression.computeCost X y theta
        let new_theta, J_history = gradientDescent X y theta alpha num_iters
        let hypothesis = [for i in 4.5 .. 25.0 -> (i,new_theta.[0] + new_theta.[1]*i)]

        let newY = sigmoid X

        let f = 
            Chart.Combine(
                [   Chart.Point(chartData, Name="Population vs Profit").WithXAxis(Max=24.0, Min=4.0)
                    Chart.Line(hypothesis)

                ]).ShowChart()
       
        let f = (Chart.Line(J_history)).ShowChart()
        *)

        let logisticData = loadWithPath "ex2data1.txt"

        let matrixLogisticData = logisticData |> matrix
        
        let sigmoidActivation = sigmoid matrixLogisticData

        // System.Windows.Forms.Application.Run(f)
        0 // return an integer exit code