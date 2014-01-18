open System
open System.Text

type ValueRange(startPoint: float, endPoint: float) =
    member this.StartPoint = startPoint
    member this.EndPoint = endPoint

    member this.Range =
        this.EndPoint - this.StartPoint

    member this.Includes value =
        this.StartPoint < value && value < this.EndPoint

    member this.Includes (range:ValueRange) =
        this.StartPoint < range.StartPoint && range.EndPoint < this.EndPoint

    member this.IncludesOrIsOn value =
        this.StartPoint <= value && value <= this.EndPoint
    
    member this.IncludesOrIsOn (range:ValueRange) =
        this.StartPoint <= range.StartPoint && range.EndPoint <= this.EndPoint

    member this.IsWithin (range:ValueRange) =
        range.Includes this

    member this.IsWithinOrIsOn (range:ValueRange) =
        range.IncludesOrIsOn this

let entireRange = ValueRange(0.0, 1.0)

type PercentageDataPoint = { Data: byte; Percent: float }

type RangeDataPoint(data: byte, range: ValueRange) =
    inherit ValueRange(range.StartPoint, range.EndPoint)
    member this.Data = data

type DataPoint =
     | Percentage of PercentageDataPoint
     | Range of RangeDataPoint

module Percentages =
    let rec private iter (output:RangeDataPoint list) (input:PercentageDataPoint list) =
        match (output, input) with
        | (output, []) -> output
        | ([], cur::input) -> 
            let ret = RangeDataPoint(cur.Data, ValueRange(0.0, cur.Percent))
            iter [ret] input
        | (prev::output, cur::input) ->
            let startPoint = prev.EndPoint
            let endPoint = prev.EndPoint + cur.Percent
            let range = ValueRange(startPoint, endPoint)

            let ret = RangeDataPoint(cur.Data, range)
            let output = ret :: prev :: output
            assert (output.Length >= 2)
            iter output input

    let toRanges (percentages:PercentageDataPoint list) = iter [] percentages

module ValueRanges =
    let startPointSort<'T when 'T :> ValueRange> =
        fun (range:ValueRange) -> range.StartPoint

    let endPointSort<'T when 'T :> ValueRange> =
        fun (range:ValueRange) -> range.EndPoint

    let sortBy<'T when 'T :> ValueRange> sortMethod (ranges:'T list) =
        ranges |> List.sortBy sortMethod

    let check<'T when 'T :> ValueRange> (ranges:'T list) =
        assert (ranges.Length > 0)
        let ranges = ranges |> List.sortBy startPointSort

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
        |> List.map (fun x -> { Data = fst x; Percent = snd x })

module Encoder =
    let private findRangeFromData data (ranges:RangeDataPoint list) =
        ranges |> List.find (fun r -> r.Data = data)

    let private getNextRange rangeMapping data (currentRange:ValueRange) =
        let range = currentRange.Range
        let nextRange = rangeMapping |> findRangeFromData data

        let startPoint = currentRange.StartPoint + (range * nextRange.StartPoint)
        let endPoint = currentRange.StartPoint + (range * nextRange.EndPoint)

        assert (startPoint < endPoint)
        assert (currentRange.IncludesOrIsOn startPoint)
        assert (currentRange.IncludesOrIsOn endPoint)

        ValueRange(startPoint, endPoint)

    let rec private iter buffer rangeMapping (range:ValueRange) =
        match buffer with
        | [] ->
            ValueRange(range.StartPoint, range.EndPoint)
        | data::buffer ->
            let nextRange = getNextRange rangeMapping data range
            assert (nextRange.IsWithinOrIsOn entireRange)
            iter buffer rangeMapping nextRange

    let encode ranges (buffer:byte list) : ValueRange =
        iter buffer ranges entireRange

[<EntryPoint>]
let main argv = 
    let text = "HELLO"
    let data = Encoding.ASCII.GetBytes(text) |> Array.toList

    let ranges = data
                 |> Data.toPercentages
                 |> Percentages.toRanges

    ranges |> ValueRanges.check // optional

    let ranges = ranges |> ValueRanges.sortBy ValueRanges.startPointSort
    for item in ranges do
        printfn "%c: (%f, %f)" (char item.Data) item.StartPoint item.EndPoint
    
    let encoded = data |> Encoder.encode ranges
    printfn "Result = (%f, %f)" encoded.StartPoint encoded.EndPoint

    printfn "Done"
    Console.ReadLine() |> ignore
    0
