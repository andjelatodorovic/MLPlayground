namespace MachineLearning

open MathNet.Numerics.LinearAlgebra

module Misc = 

    let addOnesColumn (rawX : Matrix<double>) =
        (DenseMatrix.init rawX.RowCount 1 (fun _ _ -> 1.0)).Append(rawX)

