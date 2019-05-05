namespace Fennel

module IBClient =

  open IBApi
  open System
  open System.Globalization
  open System.Diagnostics.CodeAnalysis
  open System.Threading
  open Util

  type Side =
    | Buy
    | Sell
    | ShortSell

  /// <summary>
  /// Type representing different order types being supported
  /// IOC implies a limit order with TIF=IOC
  /// </summary>
  type OrderType =
    | IOC
    | Market

  /// <summary>
  /// Type representing different bar size types in the IB API for the
  /// historical data call
  /// </summary>
  type BarSize =
    | OneSec
    | FiveSecs
    | FifteenSecs
    | ThirtySecs
    | OneMin
    | TwoMins
    | ThreeMins
    | FiveMins
    | FifteenMins
    | ThirtyMins
    | OneHour
    | OneDay

  /// <summary>
  /// Type representing different duration type suffixes in the IB API for the
  /// historical data call
  /// </summary>
  type DurationType =
    | Sec
    | Day
    | Week
    | Month
    | Year

  /// <summary>
  /// Type representing different WhatToShow types in the IB API foer the
  /// historical data call
  /// </summary>
  type WhatToShowType =
    | Trades
    | Midpoint

  /// <summary>
  /// Make duration string for the IB API historical data call
  /// </summary>
  /// <param name="t">duration type</param>
  /// <param name="length">integer representing the length</param>
  let makeDurationString t length =
    let suffix = 
      match t with
      | Sec -> "S"
      | Day -> "D"
      | Week -> "W"
      | Month -> "M"
      | Year -> "Y"
    sprintf "%d %s" length suffix

  /// <summary>
  /// Make bar size string for the IB API historical data call
  /// </summary>
  /// <param name="bs">bar size</param>
  let makeBarSizeString bs =
    match bs with
    | OneSec -> "1 secs"
    | FiveSecs -> "5 secs"
    | FifteenSecs -> "15 secs"
    | ThirtySecs -> "30 secs"
    | OneMin -> "1 min"
    | TwoMins -> "2 mins"
    | ThreeMins -> "3 mins"
    | FiveMins -> "5 mins"
    | FifteenMins -> "15 mins"
    | ThirtyMins -> "30 mins"
    | OneHour -> "1 hour"
    | OneDay -> "1 day"

  /// <summary>
  /// Make WhatToShow string for the IB API historical data call
  /// </summary>
  /// <param name="wts">WhatToShow type</param>
  let makeWhatToShowString wts =
    match wts with
    | Trades -> "Trades"
    | Midpoint -> "MIDPOINT"

  /// <summary>
  /// IB API returns timestamp in either the full day format or the year month
  /// day format
  /// </summary>
  let private fullDayPattern = "yyyyMMdd  HH:mm:ss"
  let private yearMonthDayPattern = "yyyyMMdd"

  /// <summary>
  /// Try parsing a timestamp with the full day format or the year month day
  /// format, returning None if none of the format works
  /// </summary>
  /// <param name="format"></param>
  /// <param name="timestamp"></param>
  let private tryParseTimestamp (format: string) (timestamp: string) =
    if timestamp.Length <> format.Length then None else
      match DateTime.TryParseExact(timestamp, format, 
                                   CultureInfo.InvariantCulture, 
                                   DateTimeStyles.None)  with
      | true, d -> Some d
      | _ -> None

  /// <summary>
  /// Partial active pattern for timestamp in full day format
  /// </summary>
  /// <param name="timestamp"></param>
  let (|FullDayPattern|_| ) (timestamp: string) =
    tryParseTimestamp fullDayPattern timestamp
  
  /// <summary>
  /// Partial active pattern for timestamp in year month day pattern
  /// </summary>
  /// <param name="timestamp"></param>
  let (|YearMonthDayPattern|_|) (timestamp: string) =
    tryParseTimestamp yearMonthDayPattern timestamp

  /// <summary>
  /// Type representing the return value of the tick price and tick size
  /// callback.  The key to the dictionary is the field id.  The value type is
  /// a tuple representing (tickerId, value, canAutoExecute)  Note that for
  /// tick size, canAutoExecute is always set to false
  /// </summary>
  type TickEntry = {
    TickerId: int
    Field: int
    Value: float
    CanAutoExecute: bool
  }

  /// <summary>
  /// Type representing the return value of the account summary callback
  /// </summary>
  type AccountSummaryEntry = {
    ReqId: int
    Tag: string
    Account: string
    Value: string
    Currency: string
  }

  /// <summary>
  /// Type representing the return value of the historcal data callback
  /// </summary>
  type HistoricalDataEntry = {
    Date: DateTime
    Open: float
    Close: float
    High: float
    Low: float
    Volume: float
    Count: int
    Vwap: float
    HasGaps: bool
  }

  /// <summary>
  /// Type representing order status
  /// </summary>
  type AckInfo = {
    Id: int
    Status: string
    Filled: int
    Remaining: int
    AvgFillPrice: float
    PermId: int
    ParentId: int
    LastFilledPrice: float
    ClientId: int
    WhyHeld: string
  }

  let defaultAckInfo = {
    Id = -1
    Status = "NoAction"
    Filled = 0
    Remaining = 0
    AvgFillPrice = 0.0
    PermId = 0
    ParentId = 0
    LastFilledPrice = 0.0
    ClientId = 0
    WhyHeld = ""
  }

  /// <summary>
  /// Type holding information of an error message callback
  /// </summary>
  type ErrorInfo = {
    Id: int
    ErrorCode: int
    ErrorMsg: string
  }

  /// <summary>
  /// Type representing position entry
  /// </summary>
  type PositionEntry = {
    Account: string
    Contract: Contract
    Pos: int
    AvgCost: float
  }

  /// <summary>
  /// Order status is either an Ack when the order is fully filled or when its
  /// cancelled (At the moment, only Market and IOC orders are supported to
  /// make things easier.  There are no partially filled orders) or if there is
  /// an error
  /// </summary>
  type OrderStatus =
    | Ack of AckInfo * Order
    | Err of ErrorInfo * Order

  /// <summary>
  /// Maps to store multiple entries returned by multiple invocation of a 
  /// callback triggered by one single api call
  /// </summary>
  type TickMap = Map<int, TickEntry>
  type AccountSummaryMap = Map<string, AccountSummaryEntry>
  type HistoricalDataMap = Map<DateTime, HistoricalDataEntry>
  type OrderMap = Map<int, Order>
  type OrderStatusMap = Map<int, OrderStatus>
  

  /// <summary>
  /// Type representing different data types read from the socket and returned
  /// by the handler.  The handler has one single Response object
  /// </summary>
  type Response =
    | AccountSummary of AccountSummaryMap
    | ContractDetails of ContractDetails
    | CurrentTime of int64
    | NoResponse
    | Tick of TickMap
    | HistoricalData of HistoricalDataMap
    | NextValidId of int
    | OrderStatus of OrderStatus
    | Position of PositionEntry list

    static member accountSummary response =
      match response with
      | AccountSummary a -> a
      | _ -> failwith "Expecting AccountSummary response type"

    static member tick response =
      match response with
      | Tick t -> t
      | _ -> failwith "Expecting Tick response type"

    static member historicalData response =
      match response with
      | HistoricalData h -> h
      | _ -> failwith "Expecting HistoricalData response type"

    static member position response =
      match response with
      | Position p -> p
      | _ -> failwith "Expecting Position response type"

    static member orderStatus response =
      match response with
      | OrderStatus s -> s
      | _ -> failwith "Expecting OrderStatus response type"

  /// <summary>
  /// Create an IBApi Contract object
  /// </summary>
  /// <param name="symbol"></param>
  /// <param name="sectype"></param>
  /// <param name="currency"></param>
  /// <param name="exchange"></param>
  /// <param name="expiry"></param>
  let makeContract symbol sectype currency exchange expiry =
    Contract(Symbol=symbol, SecType=sectype, Currency=currency, 
               Exchange=exchange, Expiry=expiry)

  /// <summary>
  /// Extracts tick map from a response object.  Returns an empty map if the
  /// response does not actually contain a tick map
  /// </summary>
  /// <param name="response"></param>
  let getTickMap response =
    match response with
    | Tick m -> m
    | _ -> Map.empty

  /// <summary>
  /// Prints tick map
  /// </summary>
  /// <param name="m"></param>
  let printTickMap (m: TickMap) =
    m
    |> Seq.map (fun e -> sprintf "%s %A" (TickType.getField(e.Key)) e.Value)
    |> Seq.iter (printfn "%s")

  /// <summary>
  /// An implementation of the EWrapper interface which aims at making the api
  /// calls synchronous by using a ManualResetEvent.  The type has one single
  /// Response object to store any data returned by the callback functions in
  /// the EWrapper interface.  This type is passed to the EClientSocket in its
  /// contructor.  Upon calling eConnect(), connection to the socket is made.
  /// When connection is successful, a thread is spawn which reads from the
  /// socket and calls the appropriate callback functions in the EWrapper
  /// interface according to the message type id.  Therefore, all functions
  /// implementing the EWrapper interface are called from a separate thread than
  /// the thread where the EClientSocket request calls are made (presumably, 
  /// that is the main thread)  The ManualResetEvent object is used to
  /// synchronize the access of the response object between the reader thread
  /// and the writer (main) thread.  The callback functions should set the
  /// event object when it's done writing to the response object.  The main
  /// thread should call the Wait() method after calling the various api calls
  /// in the EClientSocket.  When the Wait() method returns, the response should
  /// be in the Response object.  The Wait() method will also reset the event
  /// object.  After the caller is done with the response object, it should
  /// call the ResetResponse() method to reset the response object to NoResponse
  /// </summary>
  [<SuppressMessage("NameConventions", "MemberNamesMustBePascalCase")>]
  type private SyncHandler() = 
    let mutable response = NoResponse
    let mutable nextId = -1
    let mutable (orderMap: OrderMap) = Map.empty
    let mutable (orderStatusMap: OrderStatusMap) = Map.empty
    let ev = new System.Threading.ManualResetEvent(false)

    /// <summary>
    /// The response object which stores the result of the api call
    /// </summary>
    member this.Response = response

    /// <summary>
    /// The next valid id to use.  This is not protected by any semaphore
    /// Upon accessing this field, the next id to use is automatically
    /// incremented by 1
    /// </summary>
    member this.NextId = 
      let retId = nextId
      nextId <- nextId + 1
      // printfn "After increment: %d" nextId
      retId

    /// <summary>
    /// Set the event object in the Handler.  Upon being called, one thread
    /// waiting on the event will go through.  In our context, there should
    /// just be one thread waiting.  This function is called when the reader
    /// thread is done with updating the response object with data read from
    /// the socket
    /// </summary>
    member this.SetFlag() = 
      // printfn "Setting flag..."
      ev.Set() |> ignore

    /// <summary>
    /// Reset the event object in the Handler so that it is ready for response
    /// coming back for other requests
    /// </summary>
    member this.ResetFlag() = ev.Reset() |> ignore

    /// <summary>
    /// Blocks the calling thread until the event is set.  This should be called
    /// after making an api call through the EClientSocket object.  When the
    /// call returns, the response object has been written to and can be read
    /// and also the event object has also been reset, ready for other responses
    /// </summary>
    member this.Wait() = 
      ev.WaitOne() |> ignore
      this.ResetFlag() |> ignore
      let r = this.Response
      this.ResetResponse()
      r

    /// <summary>
    /// Resets the response object to NoResponse.  This should be called when
    /// the caller thread no longer needs the response object in the handler
    /// </summary>
    member this.ResetResponse() = response <- NoResponse

    /// <summary>
    /// Insert an order into the order map
    /// </summary>
    /// <param name="id"></param>
    /// <param name="order"></param>
    member this.InsertOrder id order =
      lock orderMap (fun() -> orderMap <- orderMap.Add(id, order) )

    /// <summary>
    /// Insert an order status object into the order status map.  The order
    /// status map is only accessed within the ewrapper thread, therefore there
    /// is no need to introduce a lock, hence the private access modifier
    /// </summary>
    /// <param name="id"></param>
    /// <param name="orderStatus"></param>
    member private this.InsertOrderStatus id orderStatus =
      orderStatusMap <- orderStatusMap.Add( id, orderStatus )

    /// <summary>
    /// Look up an order in the order map given its id
    /// </summary>
    /// <param name="id"></param>
    member this.FindOrder id = 
      lock orderMap ( fun() -> orderMap.TryFind id )

    /// <summary>
    /// Look up an order status in the order status map given an order id. This
    /// method is only used in the ewrapper thread, hence the private access
    /// modifier
    /// </summary>
    /// <param name="id"></param>
    member private this.FindOrderStatus id = orderStatusMap.TryFind id
       
    member private this.setTickResponse tickerId field value 
      (canAutoExecute: int) =

        let addField (m:TickMap) = 
          m.Add(field, { Field=field; TickerId=tickerId; Value=value; 
                          CanAutoExecute=Convert.ToBoolean(canAutoExecute)} )
        response <-
          match response with
          | Tick m -> addField m |> Tick
          | _ -> Map.empty |> addField |> Tick

    interface EWrapper with
      member this.accountDownloadEnd(account: string) = this.SetFlag()

      member this.accountSummary(reqId, account, tag, value, currency) =
        let entry = { Tag=tag; ReqId=reqId; Account=account; Value=value; 
                      Currency=currency }
        response <-
          match response with
          | AccountSummary m -> m.Add(tag, entry )
          | _ -> Map.empty.Add(tag, entry)
          |> AccountSummary

      member this.accountSummaryEnd(reqId) = this.SetFlag()

      member this.bondContractDetails(reqId, contract) = ()

      member this.commissionReport(commissionReport) =
        sprintf "%A" commissionReport |> tsLog

      member this.connectionClosed() =
        sprintf "connectionClosed" |> tsLog

      member this.contractDetails(reqId, contractDetails) = ()

      member this.contractDetailsEnd(reqId) = ()

      member this.currentTime(time) =
        response <- CurrentTime time
        this.SetFlag()

      member this.deltaNeutralValidation(reqId, underComp) = ()

      member this.displayGroupList(reqId, group) = ()

      member this.displayGroupUpdated(reqId, group) = ()

      member this.error(id, errorCode, errMsg) =
        let isWarning errorCode (errMsg: string) =
          if errorCode = 399 && errMsg.Contains("Warning") then true else false

        sprintf "Error %d %d %s" id errorCode errMsg |> tsLog
        if id >= 0 && not (isWarning errorCode errMsg) then
          let e = { Id=id; ErrorCode=errorCode; ErrorMsg=errMsg }
          match this.FindOrder id with
          | Some o ->
              match this.FindOrderStatus id with
              | None -> this.InsertOrderStatus id (Err (e, o))
                        response <- OrderStatus (Err (e, o))
                        this.SetFlag()
              | _ -> sprintf "Status for order %d already received.  Ignored" id
                     |> tsLog
          | _ -> sprintf "Cannot find order with id %d in the map, weird" id
                 |> tsLog

      member this.error(msg) = failwith msg |> ignore

      member this.error(e: exn) =
        let errMsg = sprintf "%s %s\n%s" (DateTime.UtcNow.ToString()) e.Message
                       e.StackTrace
        failwith errMsg |> ignore
 
      member this.execDetails(reqId, contract, e) =
        sprintf "Exec Details for order %d: %s %s %d %f" e.OrderId 
          contract.Symbol e.Side  e.Shares e.Price |> tsLog

      member this.execDetailsEnd(reqId) = this.SetFlag()

      member this.fundamentalData(reqId, data) = ()

      member this.historicalData(reqId, timestamp, o, h, l, c, volume, count,
                                  vwap, hasGaps) =

        let date = match timestamp with
                   | FullDayPattern d -> d
                   | YearMonthDayPattern d -> d
                   | _ -> DateTimeOffset.FromUnixTimeSeconds(
                            int64 timestamp).DateTime
        let entry = { Date=date; Open=o; High=h; Low=l; Close=c; 
                      Volume=float volume; Count=count; Vwap=vwap; 
                      HasGaps=hasGaps}
        response <-
          match response with 
          | HistoricalData m -> m.Add(date, entry)
          | _ -> Map.empty.Add(date, entry)
          |> HistoricalData

      member this.historicalDataEnd(reqId, s, e) = this.SetFlag()

      member this.managedAccounts(accountsList) = ()

      member this.marketDataType(reqId, marketDataType) = ()

      member this.nextValidId(orderId) = 
        nextId <- orderId

      member this.openOrder(orderId, contract, order, orderState) = ()

      member this.openOrderEnd() = ()

      member this.orderStatus( id, status, filled, remaining, avgPrice, permId,
                               parentId, lastPrice, clientId, whyHeld) =
        let s = status.ToUpper()
        if s = "FILLED" || s = "CANCELLED" then // [TODO only mkt or ioc now]
          let ordStatus = { 
            Id=id; Status=status; Filled=filled; Remaining=remaining; 
            AvgFillPrice=avgPrice; PermId=permId; ParentId=parentId; 
            LastFilledPrice=lastPrice; ClientId=clientId; WhyHeld=whyHeld }

          match this.FindOrder id with
          | Some o ->
              match this.FindOrderStatus id with
              | None -> 
                  sprintf "oid: %d, status: %s, filled: %d, remaining: %d, avgPrice: %f"
                    id status filled remaining avgPrice |> tsLog
                  this.InsertOrderStatus id (Ack (ordStatus, o))
                  response <- OrderStatus (Ack (ordStatus, o))
                  this.SetFlag()
              | _ -> 
                  sprintf "Status for order %d already received.  Ignored" id
                  |> tsLog
          | _ -> sprintf "Cannot find order with id %d in the map, weird" id
                 |> tsLog

      member this.position(account, contract, pos, avgCost) =
        let e = {Account=account; Contract=contract; Pos=pos; AvgCost=avgCost}
        response <- 
          match response with
          | Position positions -> e::positions
          | _ -> [e]
          |> Position

      member this.positionEnd() = this.SetFlag()

      member this.realtimeBar(reqId, time, o, h, l, c, v, vwap, count) = ()

      member this.receiveFA(faDataType, faXmlData) = ()

      member this.scannerData(reqId, rank, contractDetails, distance,
                              benchmark, projection, legStr) = ()

      member this.scannerDataEnd(reqId) = ()

      member this.scannerParameters(xml) = ()

      member this.tickEFP(tickerId, tickType, basisPoints, formattedBasisPoints,
                          impliedFuture, holdDays, futureExpiry, dividendImpact,
                          dividendsToExpiry) = ()

      member this.tickGeneric(tickerId, field, value) = ()

      member this.tickOptionComputation(tickerId, field, impliedVolatility,
                                        delta, optPrice, pvDividend, gamma,
                                        vega, theta, undPrice) = ()

      member this.tickPrice(tickerId, field, price, canAutoExecute) =
        this.setTickResponse tickerId field price canAutoExecute

      member this.tickSize(tickerId, field, size) =
        this.setTickResponse tickerId field (float size) 0        

      member this.tickSnapshotEnd(tickerId) = this.SetFlag()

      member this.tickString(tickerId, field, value) = ()

      member this.updateAccountTime(timestamp) = ()

      member this.updateAccountValue(key, value, currency, accountName) = ()

      member this.updateMktDepth(tickerId, position, operation, side, price,
                                  size) = ()
      member this.updateMktDepthL2(tickerId, position, marketMaker, operation,
                                   side, price, size) = ()

      member this.updateNewsBulletin(msgId, msgType, msg, origExchange) = ()

      member this.updatePortfolio(contract, position, marketPrice, marketValue,
                                  averageCost, unrealizedPNL, realizedPNL,
                                  accountName) = ()

      member this.verifyCompleted(isSuccessful, errorText) = this.SetFlag()

      member this.verifyMessageAPI(apiData) = this.SetFlag()

  /// <summary>
  /// SyncClient is a wrapper class for EClientSocket but with all the functions
  /// returning a Response object synchronously.  It also implements the
  /// IDisposable interface so that the undelrying EClientSocket will disconnect
  /// if there is an uncaught exception
  /// </summary>
  type SyncClient() =

    let handler = SyncHandler()
    let client = EClientSocket(handler)
    
    interface IDisposable with
      /// <summary>
      /// Disconnect when disposed of
      /// </summary>
      member this.Dispose() = if client.IsConnected() then client.eDisconnect()

    member this.IsConnected() = client.IsConnected()

    member this.Connect host port = client.eConnect(host, port, 0, false)

    member this.Disconnect() = client.eDisconnect()

    /// <summary>
    /// Request market data snapshot for a contract
    /// </summary>
    /// <param name="contract"></param>
    /// <returns>TickMap response</returns>
    member this.ReqMktDataSnapshot contract =
      // use a fixed reqId for every call for now, as we are making the call
      // synchronous, so in theory there should be no chance of mixing up the
      // responses from different calls
      client.reqMktData(1, contract, "", true, null)
      handler.Wait()

    /// <summary>
    /// Requests for account summary
    /// </summary>
    /// <param name="group"></param>
    /// <param name="tags"></param>
    /// <returns>AccountSummary response</returns>
    member this.ReqAccountSummary group tags =
      client.reqAccountSummary(1, group, tags)
      handler.Wait()

    /// <summary>
    /// Requests for the curren time
    /// </summary>
    member this.ReqCurrentTime() =
      client.reqCurrentTime()
      handler.Wait()

    /// <summary>
    /// Requests historical data for a particular contract
    /// </summary>
    /// <param name="tickerId"></param>
    /// <param name="contract"></param>
    /// <param name="endDateTimeUtc"></param>
    /// <param name="durString"></param>
    /// <param name="barSizeSetting"></param>
    /// <param name="whatToShow"></param>
    /// <param name="useRTH"></param>
    /// <returns>HistoricalDataMap response, key DateTime, value
    /// HistoricalDataEntry</returns>
    member this.ReqHistoricalData tickerId contract (endDateTimeUtc: DateTime) 
      durString barSizeSetting whatToShow useRTH = 

      let dateFormat = "yyyyMMdd hh:mm:ss \G\M\T"
      let dateString = endDateTimeUtc.ToUniversalTime().ToString(dateFormat)
      client.reqHistoricalData(1, contract, dateString, durString,
        barSizeSetting, whatToShow, useRTH, 2, null)
      handler.Wait()

    /// <summary>
    /// Returns the next valid order id to use
    /// </summary>
    member this.ReqIDs() =
      // client.reqIds 1 - no need to call reqIds any more as the incrementing
      // of the id is now maintained by the handler itself
      handler.NextId     

    /// <summary>
    /// Send an IOC order for a particular contract, quantity and price
    /// </summary>
    /// <param name="side">Buy/Sell</param>
    /// <param name="contract"></param>
    /// <param name="quantity"></param>
    /// <param name="price"></param>
    /// <returns>Order status response</returns>
    member this.SendOrder side ordType contract quantity price =
      let id = this.ReqIDs()
      let o = Order()
      o.TotalQuantity <- quantity
      if ordType = IOC then
        o.LmtPrice <- price
        o.OrderType <- "LMT"
        o.Tif <- "IOC"
      else
        o.OrderType <- "MKT"
        o.LmtPrice <- 0.0
      o.Action <- match side with
                  | Buy -> "BUY"
                  | Sell -> "SELL"
                  | ShortSell -> "SSHORT"
      handler.InsertOrder id o
      client.placeOrder(id, contract, o)
      sprintf "Placed order: %d %s %s %d @ %f" id o.Action contract.Symbol 
        o.TotalQuantity o.LmtPrice |> tsLog
      handler.Wait()

    member this.ReqPositions() =
      client.reqPositions()
      handler.Wait()

  module UnitTests =
  
    open NUnit.Framework
    open FsUnit
    open Fennel.Util

    [<TestCase("127.0.0.1", 4002)>]
    let ``IB Connection``(host, port) =
      use client = new SyncClient()
      // Assume ib gateway started and waiting on port 4001
      client.Connect host port
      client.IsConnected() |> should equal true
      let nextId = client.ReqIDs()
      let nextId' = client.ReqIDs()
      printfn "1 - Next id: %d" nextId
      printfn "2 - Next id: %d" nextId'
      nextId' - nextId |> should equal 1
      client.Disconnect()
      client.IsConnected() |> should equal false

    [<TestCase("127.0.0.1", 4002, "$LEDGER:USD")>]
    let ``Request Account Summary``(host, port, tag) =
      use client = new SyncClient()
      // Assume ib gateway started and waiting on port 4002
      client.Connect host port
      client.IsConnected() |> should equal true
      if client.IsConnected() then
        let r = client.ReqAccountSummary "All" tag
        match r with
        | AccountSummary m -> 
          m 
          |> Seq.map (fun e -> sprintf "%s %A" e.Key e.Value)
          |> Seq.iter (fun s -> printfn "%s" s)
        | _ -> printfn "Not an account summary"

    [<TestCase("127.0.0.1", 4002)>]
    let ``Request Current Time``(host, port) =
      use client = new SyncClient()
      // Assume ib gateway started and waiting on port 4002
      client.Connect host port
      client.IsConnected() |> should equal true
      printfn "Next id: %d" (client.ReqIDs())
      let r = client.ReqCurrentTime()
      printfn "Next id: %d" (client.ReqIDs())
      let ibTime = 
        match r with 
        | CurrentTime time -> time
        | _ -> 0L
      let sysTime = 
        int64 (DateTime.UtcNow.Subtract(DateTime(1970,1,1)).TotalSeconds)
      ibTime |> should (equalWithin 10) sysTime
   
    [<TestCase("127.0.0.1", 4002, "201612")>]
    let ``ReqMarketDataSnapshot``(host, port, expiry) =
      let nkyContract = makeContract "N225" "IND" "JPY" "OSE.JPN" ""
      let n225mContract = makeContract "N225M" "FUT" "JPY" "OSE.JPN" expiry
      let usdjpyContract = makeContract "USD" "CASH" "JPY" "IDEALPRO" ""
      use client = new SyncClient()
      client.Connect host port
      client.IsConnected() |> should equal true
      [nkyContract; n225mContract; usdjpyContract]
      |> List.map (client.ReqMktDataSnapshot >> getTickMap)
      |> List.iter printTickMap

    [<TestCase("127.0.0.1", 4002)>]
    let ``Req Historical``(host, port) =
      let contract = makeContract "USD" "CASH" "JPY" "IDEALPRO" ""
      use client = new SyncClient()
      client.Connect host port
      client.IsConnected() |> should equal true
      let r = client.ReqHistoricalData 1 contract (DateTime.Now) "30 S" "1 secs" 
                "MIDPOINT" 1
      printfn "%A" r

    [<TestCase("127.0.0.1", 4002)>]
    let ``Req Next Order Id``(host, port) =
      use client = new SyncClient()
      client.Connect host port
      client.IsConnected() |> should equal true
      let r = client.ReqIDs()
      printfn "%A" r

//    [<TestCase("127.0.0.1", 4002, 0, 0)>]
//    [<TestCase("127.0.0.1", 4002, 1, 0)>]
//    [<TestCase("127.0.0.1", 4002, 0, 1)>]
//    [<TestCase("127.0.0.1", 4002, 1, 1)>]
//    let ``Send IOC Order``(host, port, side, ordType) =
//      let contract = makeContract "USD" "CASH" "JPY" "IDEALPRO" ""
//      let minTick = 0.005
//      let minQty = 25000
//      use client = new SyncClient()
//      client.Connect host port
//      client.IsConnected() |> should equal true
//      let m = contract |> client.ReqMktDataSnapshot |> getTickMap
//      printTickMap m
//      let bid = m.Item (TickType.BID)
//      let ask = m.Item (TickType.ASK)
//      let ordSide = if side = 0 then Buy else Sell
//      let v = if side = 0 then ask.Value else bid.Value
//      let px = if side = 0 then roundUp v minTick else roundDown v minTick
//      let s = sprintf "side: %A, price: %f, order px: %f" ordSide v px
//      printfn "%s" s
//      let otype = if ordType = 0 then IOC else Market
//      let r = client.SendOrder ordSide otype contract minQty px
//      printfn "%A" r
      
    [<TestCase(1, "D")>]
    [<TestCase(30, "S")>]
    [<TestCase(1, "Y")>]
    let ``Make duration string``(length, durationTypeString) =
      let durationType = match durationTypeString with
                         | "S" -> Sec
                         | "D" -> Day
                         | "M" -> Month
                         | "Y" -> Year
                         | _ -> failwith "Duration string not recognized"
      let result = makeDurationString durationType length
      let expected = sprintf "%d %s" length durationTypeString
      result |> should equal expected

    [<TestCase("127.0.0.1", 4002)>]
    let ``Request Position``(host, port) =
      use client = new SyncClient()
      client.Connect host port
      if client.IsConnected() then
        client.ReqPositions()
        |> Response.position
        |> List.iter (fun e -> sprintf "%s %s %s %s %d %f" e.Account 
                                 e.Contract.Symbol e.Contract.Expiry
                                 e.Contract.Currency
                                 e.Pos e.AvgCost |> printf "%s")
