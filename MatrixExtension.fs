namespace MathNet.Numerics.LinearAlgebra

module Vector =
  let inline ofSeq s = s |> Seq.toList |> vector
  let inline join v1 v2 =
    let v1List = v1 |> Vector.toList
    v2 |> Vector.toList |> List.append v1List |> vector

  let inline sigmoid (x: Vector<float>) = 1.0 / ( 1.0 + exp (-x))

module Matrix =
  let inline aggCols f m = m |> Matrix.toColSeq |> Seq.map f |> Vector.ofSeq

  let inline aggRows f m = m |> Matrix.toRowSeq |> Seq.map f |> Vector.ofSeq

  let inline replicateRows n v = v|> Vector.toList |> List.replicate n |> matrix

  let inline replicateCols n v = replicateRows n v |> Matrix.transpose

  let inline ofRowVector v = v |> replicateRows 1

  let inline ofColVector v = v |> replicateCols 1

  let inline applyRowVector op v m = 
    v |> replicateRows (Matrix.rowCount m) |> op m

  let inline applyColVector op v m =
    v |> replicateCols (Matrix.columnCount m) |> op m

  let inline prependRowOnes (x: Matrix<float>) =
    DenseVector.create (x.ColumnCount) 1.0
    |> Matrix.prependRow
    <| x  

  let inline prependColumnOnes (x: Matrix<float>) =
    DenseVector.create (x.RowCount) 1.0
    |> Matrix.prependCol
    <| x  

  let inline sigmoid (x: Matrix<float>) = 1.0 / ( 1.0 + exp(-x) )

  let inline ofSeq r c s = s |> Seq.toArray |> DenseMatrix.raw r c

  let inline map2 f (m1: #Matrix<'a>) (m2: #Matrix<'a>) = 
    m1.Map2((fun x y -> f x y), m2)

  /// <summary>
  /// Reshape an array of numbers into a sequence of Matrix, given the dimension
  /// of each of the matrix in the sequence.  Each element in the source array
  /// is copied into the instances of matrix created
  /// </summary>
  /// <param name="src">An array of numbers</param>
  /// <param name="dimSeq">Sequence of dimensions, each dimension is a pair
  /// of integers specifying the number of rows and columns of each matrix
  /// </param>
  let reshape dimSeq (src: 'a[]) =
    let index = dimSeq |> Seq.scan (fun t (r, c) -> t + r * c) 0 |> Seq.toArray
    let size = index |> Array.last
    if src.Length < size then failwith "Source does not have enough elements"
    dimSeq
    |> Seq.mapi (
        fun i (r, c) ->
          let sz = r * c
          let dst = Array.create sz Matrix<'a>.Zero
          Array.blit src index.[i] dst 0 sz
          dst |> DenseMatrix.raw r c    // column-major
          // do not modify dst beyond this line, dst is directly referenced in
          // DenseMatrix and is not copied
        )

  /// <summary>
  /// Reshapes a vector into a sequence of Matrix, given the dimension of each
  /// of the matrix.  It simply converts the vector to an array and calls
  /// reshape
  /// </summary>
  /// <param name="v">Vector of numbers</param>
  /// <param name="dimSeq">Sequence of dimensions, each dimension is a pair
  /// of integers specifying the number of rows and columns of each matrix
  /// </param>
  let reshapeVector dimSeq (v: Vector<'a>) =
    v |> Vector.toArray |> reshape dimSeq

  /// <summary>
  /// Unrolls a sequence of matrices into one single array (column-major)
  /// Elements in the matrix are copied to the array.
  /// </summary>
  /// <param name="ms">Sequence of matrices</param>
  let unroll (ms: seq<Matrix<'a>>) =
    let index = 
      ms 
      |> Seq.scan (fun total m->total + m.RowCount * m.ColumnCount) 0
      |> Seq.toArray
    let size = index |> Array.last
    let dst = Array.create size Matrix<'a>.Zero
    ms |> Seq.iteri (
            fun i m -> 
              let src = 
                match m with
                // comments out for now - for performance
                // | :? Double.DenseMatrix as dm -> dm.Values
                | _ -> m.ToColumnMajorArray()
              Array.blit src 0 dst index.[i] src.Length)
    dst

  /// <summary>
  /// Unrolls the elements of a sequence of matrix column-major-wise into a
  /// vector
  /// </summary>
  /// <param name="ms"></param>
  let unrollIntoVector (ms: seq<Matrix<'a>>) = ms |> unroll |> vector

module UnitTests =

  open NUnit.Framework
  open FsUnit
  open MathNet.Numerics.LinearAlgebra

  [<TestCase()>]
  let ``Unroll and reshape``() =
    let rng = System.Random()
    let makeRandomMatrix (rng: System.Random) =
      DenseMatrix.randomStandard<float> (rng.Next(1, 100)) (rng.Next(1, 100))
    let ms =
      [0..rng.Next(1, 10)]
      |> List.map (fun _ -> makeRandomMatrix rng)
    let dims = ms |> List.map (fun m -> m.RowCount, m.ColumnCount)
    let unrolled = ms |> Matrix.unroll
    let reconstructed = unrolled |> Matrix.reshape dims
    reconstructed |> should equal ms