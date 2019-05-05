module Frame

  open Deedle
  open System

  /// <summary>
  /// 
  /// </summary>
  /// <param name="n"></param>
  /// <param name="newKeyName"></param>
  /// <param name="frame"></param>
  /// <param name="keys"></param>
  let getNthRowAfterKeys n newKeyName (frame: Frame<'r, 'c>) keys =
    Seq.zip frame.RowKeys frame.RowKeys 
    |> series 
    |> Frame.addCol newKeyName <| frame
    |> Frame.shift -n 
    |> Frame.sliceRows keys

  /// <summary>
  /// Apply a function to a column of a frame given the name of the column
  /// </summary>
  /// <param name="colName"></param>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyCol colName f (frame: Frame<'r, 'c>) = 
    frame |> Frame.getCol colName |> f

  /// <summary>
  /// Apply a function to the "Close" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyClose f frame = applyCol "Close" f frame

  /// <summary>
  /// Apply a function to the "Open" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyOpen f frame = applyCol "Open" f frame

  /// <summary>
  /// Apply a function to the "Low" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyLow f frame = applyCol "Low" f frame

  /// <summary>
  /// Apply a function to the "High" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyHigh f frame = applyCol "High" f frame

  /// <summary>
  /// Apply a function to the "Volume" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyVolume f frame = applyCol "Volume" f frame

  /// <summary>
  /// Apply a function to the "AdjClose" column of a frame
  /// </summary>
  /// <param name="f"></param>
  /// <param name="frame"></param>
  let applyAdjClose f frame = applyCol "AdjClose" f frame
  