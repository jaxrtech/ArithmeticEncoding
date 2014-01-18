open System
open System.Text

type DataPointPercentage = { Data: byte; Percentage: float }

[<AutoOpen>]
module RangeTypes =
    type IRangeable =
        abstract member StartPoint : float with get
        abstract member EndPoint : float with get
        abstract member Range : unit -> float

    type SelectionRange =
        { StartPoint: float; EndPoint: float; }
        interface IRangeable with
            member this.StartPoint = this.StartPoint
            member this.EndPoint   = this.EndPoint
            member this.Range()    = this.EndPoint - this.StartPoint
         
    type DataPointRange =
        { Data: byte; StartPoint: float; EndPoint: float; }
        interface IRangeable with
            member this.StartPoint = this.StartPoint
            member this.EndPoint   = this.EndPoint
            member this.Range()    = this.EndPoint - this.StartPoint

    type Range =
         | SelectionRange of SelectionRange
         | DataPointRange of DataPointRange
         interface IRangeable with
            member this.StartPoint =
                match this with
                | SelectionRange r -> r.StartPoint
                | DataPointRange r -> r.StartPoint

            member this.EndPoint =
                match this with
                | SelectionRange r -> r.EndPoint
                | DataPointRange r -> r.EndPoint

            member this.Range() =
                match this with
                | SelectionRange r -> r.EndPoint
                | DataPointRange r -> r.EndPoint

type IRangeable with
    member this.Includes value =
        this.StartPoint < value && value < this.EndPoint

    member this.Includes (range:IRangeable) =
        this.StartPoint < range.StartPoint && range.EndPoint < this.EndPoint

    member this.IncludesOrIsOn value =
        this.StartPoint <= value && value <= this.EndPoint
    
    member this.IncludesOrIsOn (range:IRangeable) =
        this.StartPoint <= range.StartPoint && range.EndPoint <= this.EndPoint

    member this.IsWithin (range:IRangeable) =
        range.Includes this

    member this.IsWithinOrIsOn (range:IRangeable) =
        range.IncludesOrIsOn this

let entireRange : IRangeable =
    SelectionRange { StartPoint = 0.0; EndPoint = 1.0 }
    :> IRangeable

module Percentages =
    let rec private iter (finished:DataPointRange list) waiting =
        match (finished, waiting) with
        | (finished, []) -> finished
        | ([], (cur:DataPointPercentage)::waiting) -> 
            let ret = { Data = cur.Data;
                        StartPoint = 0.0;
                        EndPoint = cur.Percentage }
            iter [ret] waiting
        | (prev::finished, cur::waiting) ->
            let ret = { Data = cur.Data;
                        StartPoint = prev.EndPoint;
                        EndPoint = prev.EndPoint + cur.Percentage }
            let finished = ret :: prev :: finished
            assert (finished.Length >= 2)
            iter finished waiting

    let toRanges percentages = iter [] percentages

module Ranges =
    let check ranges =
        let ranges = ranges |> List.sortBy (fun r -> r.StartPoint)
        
        // NOTE: ranges is a list this is only good for checking anyways
        let startPoint = ranges.Head.StartPoint
        let endPoint = let p = ranges.[ranges.Length - 1].EndPoint
                       Math.Round(p, 10) // account for floating point error
        assert (startPoint = 0.0)
        assert (endPoint = 1.0)

module Data =
    let private count data =
        data |> Seq.countBy id |> Seq.toList

    let private total counts =
        counts |> List.sumBy (fun x -> snd x)

    let toPercentages data =
        let counts = count data
        let total = total counts

        counts
        |> List.map (fun x -> (fst x, float (snd x) / float total))
        |> List.map (fun x -> { Data = fst x; Percentage = snd x })

module Encoder =
    let private findRangeFromData data ranges =
        ranges |> List.find (fun r -> r.Data = data)

    let private getNextRange rangeMapping data (currentRange:IRangeable) =
        let range = currentRange.Range()
        let nextRange = rangeMapping |> findRangeFromData data

        let startPoint = currentRange.StartPoint + (range * nextRange.StartPoint)
        let endPoint = currentRange.StartPoint + (range * nextRange.EndPoint)
        assert (startPoint < endPoint)
        assert (currentRange.IncludesOrIsOn startPoint)
        assert (currentRange.IncludesOrIsOn endPoint)

        SelectionRange { StartPoint = startPoint; EndPoint = endPoint }

    let rec private iter buffer rangeMapping (range:IRangeable) =
        match buffer with
        | [] ->
            SelectionRange { StartPoint = range.StartPoint; EndPoint = range.EndPoint }
            :> IRangeable
        | data::buffer ->
            let nextRange = getNextRange rangeMapping data range
            assert (nextRange.IsWithinOrIsOn entireRange)
            iter buffer rangeMapping nextRange

    let encode (ranges:DataPointRange list) (buffer:byte list) : IRangeable =
        iter buffer ranges entireRange

[<EntryPoint>]
let main argv = 
    let text = "HELLO"
    let data = Encoding.ASCII.GetBytes(text) |> Array.toList

    let ranges = data |> Data.toPercentages |> Percentages.toRanges
    ranges |> Ranges.check

    for item in ranges do
        printfn "%c: (%f, %f)" (char item.Data) item.StartPoint item.EndPoint
    
    let encoded = data |> Encoder.encode ranges
    printfn "Result = (%f, %f)" encoded.StartPoint encoded.EndPoint

    printfn "Done"
    Console.ReadLine() |> ignore
    0
