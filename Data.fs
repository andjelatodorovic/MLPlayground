namespace Fennel

module Data =

  type Bar = {
    d: System.DateTime
    h: float
    l: float
    o: float
    c: float
    v: int64
    adj: float option
  }

  /// <summary>Similar to fold, but with a more general state argument and
  /// collects the intermediate accumulator values into a list</summary>
  /// <param name="f">function that transitions the state, given the current
  /// state and the next value, it returns a pair (newState, newAccumulator)
  /// </param>
  /// <param name="state">the initial state</param>
  /// <param name="l">the list to be processed</param>
  /// <returns>the intermediate accumulator values in a list</returns>
  let rec foldState f state acc l =
    match l with
    | [] -> List.rev acc
    | h::t ->
        let state', a = f state h
        foldState f state' (a::acc) t
