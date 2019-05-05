namespace Fennel

module Plot =
  
  open Deedle
  open XPlot
  open XPlot.GoogleCharts.Deedle

  /// <summary>
  /// Create a PlotlyChart given a sequence of Deedle series
  /// </summary>
  /// <param name="series"></param>
  let createSeriesPlot (series: seq<Series<'k, 'v>>) =
    series
    |> Seq.map (fun s -> Plotly.Graph.Scatter(x = s.Keys, y = s.Values))
    |> Plotly.Chart.Plot

  /// <summary>
  /// Combines a few options into one simple call to show a PlotlyChart
  /// </summary>
  /// <param name="width"></param>
  /// <param name="height"></param>
  /// <param name="labels"></param>
  /// <param name="plot"></param>
  let showChart width height labels plot = 
    let chart = 
      plot
      |> Plotly.Chart.WithHeight height
      |> Plotly.Chart.WithWidth width
    match labels with
    | None -> chart
    | Some names -> chart |> Plotly.Chart.WithLabels names
    |> Plotly.Chart.Show

  /// <summary>
  /// Displays a Deedle data frame in a table
  /// </summary>
  /// <param name="df"></param>
  let displayFrame (df: Frame<'K, 'V>) =
    // let options = 
    //   GoogleCharts.Configuration.Options( page="enable", pageSize=20 )
    df
    |> GoogleCharts.Chart.Table
    // |> GoogleCharts.Chart.WithOptions options
    |> GoogleCharts.Chart.Show



