#r @"bin\Debug\MathNet.Numerics.dll"
#r @"bin\Debug\MathNet.Numerics.FSharp.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Text.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Matlab.dll"
#r @"bin\Debug\DotNumerics.dll"
#r @"bin\Debug\Fennel.dll"

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Matlab
open MathNet.Numerics
open DotNumerics.Optimization
open Fennel.ML
open System.IO

Control.NativeProviderPath <- Path.Combine [|__SOURCE_DIRECTORY__; @"bin\Debug"|]
Control.UseNativeMKL();;

let path = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex3/ex3/ex3data1.mat"
let thetaPath = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex3/ex3/allTheta.mat"
let data = MatlabReader.ReadAll<double>(path)
let X0 : Matrix<float> = data.["X"]
let y = data.["y"].[0.., 0]
let X = X0 |> Matrix.prependColumnOnes
let nPatterns = 10
let m = X.RowCount
let n = X.ColumnCount
let lambda = 0.1
let alpha = 1.0
let rand = System.Random()
let tolerance = 0.00001

let inline (.=) m f =
    Matrix.map (((=) f) >> (fun b -> if b then 1.0 else 0.0)) m

let predict (allTheta: Matrix<float>) (v: Vector<float>) =
  let v' = Vector.join (vector [1.0] ) v
  allTheta * v'
  |> Vector.sigmoid
  |> (fun v -> (v |> Vector.maxIndex) + 1, v |> Vector.max)

let trainGradientDescent n lambda (x: Matrix<float>) (y: Vector<float>) alpha k =
  let th0 = DenseVector.create (x.ColumnCount) 0.0
  let y1 = y |> Vector.map ((=) (float k) >> boolToFloat)
  gradientDescent n alpha (logisticCostGrad lambda X y1) th0
   
let trainBFGS lambda (x: Matrix<float>) (y: Vector<float>) k =
  let th0 = DenseVector.create (x.ColumnCount) 0.0 |> Vector.toArray
  let y' = y |> Vector.map ((=) (float k) >> boolToFloat)
  let costGrad = logisticCostGrad lambda x y' |> toArrayCostGradFunc
  (costGrad, th0) |> bfgs1 tolerance

let accuracy (y: Vector<float>) (yhat: Vector<float>) =
  let m = y.Count
  yhat - y |> abs |> Vector.map (sign >> float) |> Vector.sum
  |> (fun f -> 1.0 - f/float m)
  
let checkAccuracy x y (allTheta: Matrix<float>) =
  let yhat = 
    x * allTheta.Transpose() 
    |> Matrix.sigmoid 
    |> Matrix.aggRows (Vector.maxIndex >> float >> ((+) 1.0))
  yhat |> accuracy y

let allTheta =
  if System.IO.File.Exists(thetaPath) then
    MatlabReader.Read<float>(thetaPath, "allTheta")
  else
    [1..10]
    |> List.map (trainGradientDescent 400 0.1 X y 1.0 )
    |> List.map fst
    |> matrix

let allThetaBfgs =
  if System.IO.File.Exists(thetaPath) then
    MatlabReader.Read<float>(thetaPath, "allThetaBfgs")
  else
    [1..10]
    |> List.map (trainBFGS lambda X y )
    |> matrix

rand.Next(5000) |> (fun i -> X0.[i, 0..] |> predict allTheta, y.[i])

allThetaBfgs |> checkAccuracy X y;;
allTheta |> checkAccuracy X y;;

let trainTask lambda (x: Matrix<float>) (y: Vector<float>) k =
  async { return trainBFGS lambda x y k }

let run() =
  [1..10] 
  |> List.map (trainTask lambda X y)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> matrix
