#load "packages/Deedle/Deedle.fsx"
#r @"bin\Debug\MathNet.Numerics.dll"
#r @"bin\Debug\MathNet.Numerics.FSharp.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Text.dll"
#r @"bin\Debug\Fennel.dll"

open System
open Deedle
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open Fennel.ML

let linearRegress (x: Series<'a, float>) y =
  Series.zipInner x y
  |> Series.values
  |> MathNet.Numerics.LinearRegression.SimpleRegression.Fit

let grad f g x y = f x y, g x y

// reading a csv file
let data = DelimitedReader.Read<float>( "/Users/panga/Dropbox/Machine Learning/machine-learning-ex1/ex1/ex1data2.txt", 
             false, ",", false)
let m = data.RowCount
let n = data.ColumnCount
// append column of all 1.0 as x0
let (mu, sigma, X') = data.[0.., 0..(n-2)] |> featureNormalize
let X = DenseVector.create m 1.0 |> Matrix.prependCol <| X'
let y = data.[0.., (n-1)]
let theta0 = vector [0.0; 0.0; 0.0]
let theta = gradientDescent 1500 0.01 (linearGrad 0.0 X y) theta0 |> fst
let v = vector [1650.0; 3.0]
(v - mu)/sigma |> Vector.join (vector [1.0]) |> (*) theta
