namespace Fennel

module Queue =

  type Queue<'t> =
    | FiniteQueue of int * int * ('t list) * ('t list)

  let push q item =
    match q with
    | FiniteQueue (0, _, _, _ ) -> (q, None)
    | FiniteQueue (n, 0, [], []) -> (FiniteQueue (n, 1, [item], []), None)
    | FiniteQueue (n, _, [], []) -> failwith "Invariant violated: wrong size"
    | FiniteQueue (_, _, [], _::_) -> failwith "Invariant violated: empty front"
    | FiniteQueue (n, k, fh::ft, [] ) ->
      if ( k < n ) then
        (FiniteQueue (n, k+1, fh::ft, [item]), None)
      else
        (FiniteQueue (n, k, ft, [item]), Some fh)
    | FiniteQueue (n, k, fh::ft, r) ->
      if ( k < n ) then
        (FiniteQueue (n, k+1, fh::ft, item::r), None)
      else
        if ft = [] then
          (FiniteQueue (n, k, List.rev r, [item]), Some fh)
        else
          (FiniteQueue (n, k, ft, item::r), Some fh)

  let pop q =
    match q with
    | FiniteQueue (0, _, _, _ ) -> (q, None)
    | FiniteQueue (_, _, [], []) -> (q, None)
    | FiniteQueue (_, _, [], _::_) -> failwith "Invariant violated: empty front"
    | FiniteQueue (n, k, [item], r) ->
        (FiniteQueue (n, k-1, List.rev r, []), Some item)
    | FiniteQueue (n, k, fh::ft, r) -> (FiniteQueue (n, k-1, ft, r), Some fh)

  let pushIgnore q item = push q item |> fst

  let popIgnore q = pop q |> fst

  let last lst =
    let rec last' lst acc =
      match lst with
      | [] -> acc
      | h::t -> last' t (Some h)
    last' lst None  

  let pokeHead q =
    match q with
    | FiniteQueue (0, _, _, _) -> None
    | FiniteQueue (_, 0, [], _) -> None
    | FiniteQueue (_, _, [], _) -> failwith "Invariant violated: empty front"
    | FiniteQueue (_, _, fh::_, _) -> Some fh

  let pokeBack q =
    match q with
    | FiniteQueue (0, _, _, _) -> failwith "Queue size is zero"
    | FiniteQueue (_, 0, [], []) -> None
    | FiniteQueue (_, _, [], []) -> failwith "Invariant violated: empty front"
    | FiniteQueue (_, _, f, []) -> last f
    | FiniteQueue (_, _, _, rh::_) -> Some rh

  let toSeq = function
    | FiniteQueue (0, _, _, _) -> Seq.empty
    | FiniteQueue (_, _, [], []) -> failwith "Invariant violated: empty front"
    | FiniteQueue (_, _, f, r) -> (List.append f (List.rev r)) |> List.toSeq

  let size (FiniteQueue (_, n, _, _ )) = n
  let capacity (FiniteQueue (n, _, _, _)) = n
  let makeQueue n = FiniteQueue (n, 0, [], [])
  let makeQueueWithItem n item = pushIgnore (makeQueue n) item
