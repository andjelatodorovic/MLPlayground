#load "packages/Deedle.1.2.5/Deedle.fsx"
#r @"bin\Debug\MathNet.Numerics.dll"
#r @"bin\Debug\MathNet.Numerics.FSharp.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Text.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Matlab.dll"
#r @"bin\Debug\DotNumerics.dll"
#r @"bin\Debug\QuantFin.dll"

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Storage
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Matlab
open MathNet.Numerics
open DotNumerics.Optimization
open Deedle
open QuantFin.ML
open System.IO

Control.NativeProviderPath <- Path.Combine [|__SOURCE_DIRECTORY__; @"bin\Debug"|]
Control.UseNativeMKL();;

module Tuple =
  let map f (x, y) = (f x, f y)

let split (f1, f2) x = (f1 x, f2 x)
let iris = Frame.ReadCsv(@"C:\Users\panga\Downloads\Iris.csv")
let species = iris.GetColumn<string>("Species")
let (speciesToFactor, factorToSpecies) =
  species
  |> Series.values
  |> Set.ofSeq
  |> split (Seq.mapi (fun i v -> (v, i)), Seq.mapi (fun i v -> (i, v)))
  |> fun (x, y) -> (Map.ofSeq x), (Map.ofSeq y)
  |> fun (x, y) -> ((fun v -> float x.[v]), (fun i -> y.[i]))
let speciesFactor = species |> Series.mapValues speciesToFactor
let dataSet = 
  iris 
  |> Frame.dropCol "Species" 
  |> Frame.dropCol "Id"
  |> Frame.addCol "SpeciesFactor" speciesFactor
  |> Frame.toArray2D
  |> DenseMatrix.ofArray2

let hidden = [100]
let lambda = 1.0
let epsilon = 0.001     // used in random initialization
let tolerance = 0.0001  // used in BFGS optimization
let trainingPerc = 0.8
let testPerc = 0.2
let useNormalization = false
let randomize = true

runNN hidden lambda epsilon trainingPerc testPerc tolerance useNormalization 
  randomize dataSet