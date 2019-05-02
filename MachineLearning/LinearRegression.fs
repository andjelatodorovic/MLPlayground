namespace MachineLearning

open MathNet.Numerics.LinearAlgebra

module LinearRegression =
    
    let computeCost (X : Matrix<double>) (y : Vector<double>) (theta : Vector<double>) =
        let m = y.Count |> double
        let J = (1.0/(2.0*m)) * (((X*theta - y) |> Vector.map (fun x -> x*x)) |> Vector.sum)
        J

    /// <summary>
    /// The function performs the gradient descent for linear regression
    /// </summary>
    /// <param name="X">(N+1)xM matrix of training examples </param>
    /// <param name="y">Target array</param>
    /// <param name="theta">Hypothesis constant</param>
    /// <param name="alpha">Learning rate</param>
    /// <param name="num_iters">Number of iterations to run gradient descent</param>
    /// <returns>A vector of thetas of length feature space + 1</returns>
    /// <returns>A vector containing the Cost function history</returns>
    let gradientDescent (X : Matrix<double>) (y :Vector<double>) (theta : Vector<double>) alpha (num_iters : int) =
        let J_history = Vector<double>.Build.Dense(num_iters)
        let m = y.Count |> double
        theta.At(0, 0.0)
        let x =  (X.Column(0).PointwiseMultiply(X*theta-y)) |> Vector.sum
        for i in 0 .. (num_iters-1) do
            let next_theta0 = theta.[0] - (alpha / m) * ((X.Column(0).PointwiseMultiply(X*theta-y)) |> Vector.sum)
            let next_theta1 = theta.[1] - (alpha / m) * ((X.Column(1).PointwiseMultiply(X*theta-y)) |> Vector.sum)
            theta.[0] <- next_theta0
            theta.[1] <- next_theta1
            J_history.[i] <- (computeCost X y theta)

        let J_history_tuple = J_history
                            |> Vector.toList
                            |> List.mapi (fun i x -> i, x)

        (theta, J_history_tuple)
