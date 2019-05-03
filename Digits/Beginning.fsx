// This F# dojo is directly inspired by the 
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.
// The goal of the dojo will be to
// create a classifier that uses training data
// to recognize hand-written digits, and
// evaluate the quality of our classifier
// by looking at predictions on the validation data.
// This file provides some guidance through the problem:
// each section is numbered, and 
// solves one piece you will need. Sections contain
// general instructions, 
// [ YOUR CODE GOES HERE! ] tags where you should
// make the magic happen, and
// <F# QUICK-STARTER> blocks. These are small
// F# tutorials illustrating aspects of the
// syntax which could come in handy. Run them,
// see what happens, and tweak them to fit your goals!
// 0. GETTING READY
// Create a new F# Library project, and 
// copy the entire contents of this file
// into "Script.fsx"
// <F# QUICK-STARTER> 
// With F# Script files (.fsx) and F# Interactive,
// you can "live code" and see what happens.
// Try typing let x = 42 in the script file, 
// right-click and select "Execute in interactive".
let x = 42
// let "binds" the value on the right to a name.
// Try now typing x + 3;; in the F# Interactive window.
// ';;' indicates "execute now whatever I just typed".
// Now right-click the following 2 lines and execute:
let greet name = printfn "Hello, %s" name

// let also binds a name to a function.
// greet is a function with one argument, name.
// You should be able to run this in F# Interactive:
// greet "World";;
// </F# QUICK-STARTER> 
// Two data files are included in the same place you
// found this script: 
// trainingsample.csv, a file that contains 5,000 examples, and 
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.
// 1. GETTING SOME DATA
// First let's read the contents of "trainingsample.csv"
// We will need System and System.IO to work with files,
// let's right-click / run in interactive, 
// to have these namespaces loaded:
open System
open System.IO

module beginning =

    let trainingFile = __SOURCE_DIRECTORY__ + @"\trainingsample.csv"
    let validationFile = __SOURCE_DIRECTORY__ + @"\validationsample.csv"

    type singlePic = 
        { Number : int
          Pixels : int [] }

    let readdata file = 
        File.ReadAllLines file
        |> (fun lines -> lines.[1..])
        |> Array.map (fun x -> x.Split(','))
        |> Array.map (fun line -> Array.map (fun x -> int x) line)
        |> Array.map (fun line -> 
               { Number = line.[0]
                 Pixels = line.[1..] })

    let tdata = readdata trainingFile
    let vdata = readdata validationFile
    let dist x y = (x - y) * (x - y)

    let distance (x : int []) (y : int []) = 
        Array.map2 (fun (a : int) (b : int) -> dist a b) x y
        |> Array.sum
        |> fun x -> sqrt (float x)

    let validate (td:singlePic[]) (vd:singlePic[]) =
        Array.Parallel.map (fun (vp:singlePic) -> vp.Number, (Array.minBy(fun (tp:singlePic) -> distance vp.Pixels tp.Pixels) td).Number) vd
        |> fun arr -> (Array.length (Array.filter (fun (x, y) -> x = y) arr), Array.length arr)
        |> fun (x,y) -> (float x)/(float y)




    //
    //
    //
    //
    //let data filename = 
    //    File.ReadAllLines(filename)
    //    |> Array.map (fun line -> line.Split(','))
    //    |> fun x -> x.[1..]
    //    |> Array.map (fun line -> Array.map (fun cell -> int cell) line)
    //    |> Array.map (fun line -> 
    //           { Number = line.[0]
    //             Pixels = line.[0..] })
    //
    //let tData = data trainingFile
    //let vData = data validationFile
    //
    //let dist (x : int []) (y : int []) = 
    //    Array.map2 (fun (a : int) (b : int) -> Math.Pow(float a - float b, 2.)) x y
    //    |> Array.sum
    //    |> fun x -> sqrt x
    //
    //let validate (data : singlePic []) = 
    //    Array.Parallel.map 
    //        (fun pic -> 
    //        pic.Number, (fun (x : singlePic) -> x.Number) (Array.minBy (fun td -> dist pic.Pixels td.Pixels) tData)) data 
    //    |> (fun ar -> (float (Array.filter (fun (x, y) -> x = y) ar |> Array.length) / float (Array.length ar)))
    ////let success (x : Example) = x.Label.Equals (classify x.Pixels).Label
    ////let test (valid : Example []) = Array.map (fun (x : Example) -> success x) valid
    ////let result = test vFile
    ////  |>
    //let count (s : bool []) = s |> Array.length
    //
    //let countTrue (s : bool []) = 
    //    s
    //    |> Array.filter (fun x -> x)
    //    |> Array.length
    //
    //let div (x : int) (y : int) = float x / float y
    ////let classify (unknown : int []) = recs |> Array.minBy (fun x -> eucl x.Pixels unknown)
    //// 2. EXTRACTING COLUMNS
    //// Break each line of the file into an array of string,
    //// separating by commas, using Array.map
    //// <F# QUICK-STARTER> 
    //// Array.map quick-starter:
    //// Array.map takes an array, and transforms it
    //// into another array by applying a function to it.
    //// Example: starting from an array of strings:
    //let strings = [| "Machine"; "Learning"; "with"; "F#"; "is"; "fun" |]
    //// We can transform it into a new array,
    //// containing the length of each string:
    //let lengths = Array.map (fun (s : string) -> s.Length) strings
    //// The exact same operation above can be 
    //// done using the forward pipe operator, 
    //// which makes it look nicer:
    //let lengths2 = strings |> Array.map (fun s -> s.Length)
    //// </F# QUICK-STARTER> 
    //// The following function might help
    //let csvToSplit = "1,2,3,4,5"
    //let splitResult = csvToSplit.Split(',')
    //let sd = Array.map (fun (s : string) -> s.Split(',')) data
    //let splitData = data |> Array.map (fun s -> s.Split(','))
    //// [ YOUR CODE GOES HERE! ]
    //// 3. CLEANING UP HEADERS
    //// Did you note that the file has headers? We want to get rid of it.
    //// <F# QUICK-STARTER>  
    //// Array slicing quick starter:
    //// Let's start with an Array of ints:
    //let someNumbers = [| 0..10 |] // create an array from 0 to 10
    //// You can access Array elements by index:
    //let first = someNumbers.[0]
    //// You can also slice the array:
    //let twoToFive = someNumbers.[1..4] // grab a slice
    //let upToThree = someNumbers.[..2]
    //// </F# QUICK-STARTER> 
    //let woHeaders = splitData.[1..]
    //// 4. CONVERTING FROM STRINGS TO INTS
    //// Now that we have an array containing arrays of strings,
    //// and the headers are gone, we need to transform it 
    //// into an array of arrays of integers.
    //// Array.map seems like a good idea again :)
    //// The following might help:
    //let castedInt = (int) "42"
    //// or, alternatively:
    //let convertedInt = Convert.ToInt32("42")
    //// or, even nicer
    //let niceInt = int "42"
    //// [ YOUR CODE GOES HERE! ]
    //let ints = Array.map (Array.map (fun (s : string) -> int s)) woHeaders
    //
    //// 5. CONVERTING ARRAYS TO RECORDS
    //// Rather than dealing with a raw array of ints,
    //// for convenience let's store these into an array of Records
    //// <F# QUICK-STARTER>  
    //// Record quick starter: we can declare a 
    //// Record (a lightweight, immutable class) type that way:
    //type Example = 
    //    { Label : int
    //      Pixels : int [] }
    //
    //// and instantiate one this way:
    //let example = 
    //    { Label = 1
    //      Pixels = [| 1; 2; 3 |] }
    //
    //// </F# QUICK-STARTER>  
    //let recs = 
    //    Array.map (fun (x : int []) -> 
    //        { Label = x.[0]
    //          Pixels = x.[1..] }) ints
    //
    //// [ YOUR CODE GOES HERE! ]
    //// 6. COMPUTING DISTANCES
    //// We need to compute the distance between images
    //// Math reminder: the euclidean distance is
    //// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
    //// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))
    //// <F# QUICK-STARTER> 
    //// Array.map2 could come in handy here.
    //// Suppose we have 2 arrays:
    //let point1 = [| 0; 1; 2 |]
    //let point2 = [| 3; 4; 5 |]
    //// Array.map2 takes 2 arrays at a time
    //// and maps pairs of elements, for instance:
    //let map2Example = Array.map2 (fun p1 p2 -> p1 + p2) point1 point2
    //// This simply computes the sums for point1 and point2,
    //// but we can easily turn this into a function now:
    //let map2PointsExample (P1 : int []) (P2 : int []) = Array.map2 (fun p1 p2 -> p1 + p2) P1 P2
    //// </F# QUICK-STARTER>  
    //// Having a function like
    //let distance (p1 : int []) (p2 : int []) = 42
    //// would come in very handy right now,
    //// except that in this case, 
    //// 42 is likely not the right answer
    //let dist (p1 : int []) (p2 : int []) = Array.map2 (fun x1 x2 -> (x1 - x2) * (x1 - x2)) p1 p2
    //let edist (p1 : int []) (p2 : int []) = dist p1 p2 |> Array.reduce (fun acc item -> acc + item)
    //let eucl (p1 : int []) (p2 : int []) = sqrt (double (edist p1 p2))
    //
    //// 7. WRITING THE CLASSIFIER FUNCTION
    //// We are now ready to write a classifier function!
    //// The classifier should take a set of pixels
    //// (an array of ints) as an input, search for the
    //// closest example in our sample, and predict
    //// the value of that closest element.
    //// <F# QUICK-STARTER> 
    //// Array.minBy can be handy here, to find
    //// the closest element in the Array of examples.
    //// Suppose we have an Array of Example:
    //let someData = 
    //    [| { Label = 0
    //         Pixels = [| 0; 1 |] }
    //       { Label = 1
    //         Pixels = [| 9; 2 |] }
    //       { Label = 2
    //         Pixels = [| 3; 4 |] } |]
    //
    //// We can find for instance 
    //// the element with largest first pixel
    //let findThatGuy = someData |> Array.maxBy (fun x -> x.Pixels.[0])
    //// </F# QUICK-STARTER> 
    //// <F# QUICK-STARTER> 
    //// F# and closures work very well together
    //let immutableValue = 42
    //
    //let functionWithClosure (x : int) = 
    //    if x > immutableValue // using outside value
    //                          then true
    //    else false
    //
    //// </F# QUICK-STARTER>  
    //let validationFile = File.ReadAllLines(@"C:\Users\jovanloon\Desktop\BuildStuff\Digits\validationsample.csv")
    //let vsplitData = validationFile |> Array.map (fun s -> s.Split(','))
    //let vwoHeaders = vsplitData.[1..]
    //let vints = Array.map (Array.map (fun (s : string) -> int s)) vwoHeaders
    //
    //let vrecs = 
    //    Array.map (fun (x : int []) -> 
    //        { Label = x.[0]
    //          Pixels = x.[1..] }) vints
    //
    //let vFile = 
    //    File.ReadAllLines(@"C:\Users\jovanloon\Desktop\BuildStuff\Digits\validationsample.csv")
    //    |> Array.map (fun s -> s.Split(','))
    //    |> fun x -> x.[1..]
    //    |> Array.map (Array.map (fun x -> int x))
    //    |> Array.map (fun x -> 
    //           { Label = x.[0]
    //             Pixels = x.[1..] })
    //
    //// The classifier function should probably
    //// look like this - except that this one will
    //// classify everything as a 0:
    //let classify (unknown : int []) = recs |> Array.minBy (fun x -> eucl x.Pixels unknown)
    //let success (x : Example) = x.Label.Equals (classify x.Pixels).Label
    //let test (valid : Example []) = Array.map (fun (x : Example) -> success x) valid
    ////let result = test vFile
    ////  |>
    //let count (s : bool []) = s |> Array.length
    //
    //let countTrue (s : bool []) = 
    //    s
    //    |> Array.filter (fun x -> x)
    //    |> Array.length
    //
    //let div (x : int) (y : int) = float x / float y
    ////let perc (s:bool[]) =
    ////   s |> (Array.filter (fun x -> x) |> Array.length), Array.length
    ////  |> fun trueCount allCount -> div trueCount allCount
    //// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA
    //// Now that we have a classifier, we need to check
    //// how good it is. 
    //// This is where the 2nd file, validationsample.csv,
    //// comes in handy. 
    //// For each Example in the 2nd file,
    //// we know what the true Label is, so we can compare
    //// that value with what the classifier says.
    //// You could now check for each 500 example in that file
    //// whether your classifier returns the correct answer,
    //// and compute the % correctly predicted.
    //// [ YOUR CODE GOES HERE! ]
