namespace MachineLearning

open MathNet.Numerics.LinearAlgebra

module Activation =

    let sigmoid X =
            X |> Matrix.map (fun x -> 1.0 / (1.0 + exp x))