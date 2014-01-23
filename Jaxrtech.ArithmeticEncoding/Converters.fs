module Jaxrtech.ArithmeticEncoding.Converters

open System

/// Returns the entire range used for encoding
let entireRange = ValueRange(0N, 1N)

module PercentageDataPoints =
    /// Converts a PercentageDataPoint list to a RangeDataPoint list like set of a stacks
    let rec private iter (output:RangeDataPoint list) (input:PercentageDataPoint list) =
        match (output, input) with
        | (output, []) -> output
        | ([], cur::input) -> 
            let ret = RangeDataPoint(cur.Data, ValueRange(0N, cur.Percent))
            iter [ret] input
        | (prev::output, cur::input) ->
            let startPoint = prev.EndPoint
            let endPoint = prev.EndPoint + cur.Percent
            let range = ValueRange(startPoint, endPoint)

            let ret = RangeDataPoint(cur.Data, range)
            let output = ret :: prev :: output
            assert (output.Length >= 2)
            iter output input

    /// Converts a PercentageDataPoint list to a RangeDataPoint list in consecutive order
    let toRanges (percentages:PercentageDataPoint list) = iter [] percentages

module RangeDataPoints =
    /// Find that matching range in the range table that represents a piece of data
    let findRangeFromData data (ranges:RangeDataPoint list) =
        ranges |> List.find (fun r -> r.Data = data)

    /// Gets the next range that encode the peice of data in the current range
    let private getNextRange rangeMapping data (currentRange:ValueRange) =
        let range = currentRange.Range
        let nextRange = rangeMapping |> findRangeFromData data

        let startPoint = currentRange.StartPoint + (range * nextRange.StartPoint)
        let endPoint = currentRange.StartPoint + (range * nextRange.EndPoint)

        assert (startPoint < endPoint)
        assert (currentRange.ContainsOrIsOn startPoint)
        assert (currentRange.ContainsOrIsOn endPoint)

        ValueRange(startPoint, endPoint)

    /// Encodes the buffer using a range table and a starting range
    let rec private iter buffer rangeMapping (range:ValueRange) =
        match buffer with
        | [] ->
            ValueRange(range.StartPoint, range.EndPoint)
        | data::buffer ->
            let nextRange = getNextRange rangeMapping data range
            assert (nextRange.IsWithinOrIsOn entireRange)
            iter buffer rangeMapping nextRange
    
    // Performs arithmetic encoding a byte list to translate it to a range
    // using a range table
    let encode (buffer:byte list) ranges =
        iter buffer ranges entireRange

module Data =
    // Returns a tuple count of the number of occurances of a item in a sequence
    let count data =
        data |> Seq.countBy id |> Seq.toList

    // Returns a total of the number of occurances made using `count`
    let total counts =
        counts |> List.sumBy (fun x -> snd x)

    // Converts a byte seq to a PercentageDataPoint list
    let toPercentages data =
        let counts = count data
        let total = total counts
        
        counts
        |> List.map (fun x -> (fst x, (bignum.FromInt (snd x)) / (bignum.FromInt (total))))
        |> List.map (fun x -> { Data = fst x; Percent = snd x })

    let toRangeDataPoints buffer = 
        buffer
        |> toPercentages
        |> PercentageDataPoints.toRanges

module Point =
    let rec private iter (decoded:byte list) (rangeTable:RangeDataPoint list) (point:bignum) (length:int) =
        match length with
        | 0 -> decoded
        | _ ->
            let symbol = rangeTable
                         |> List.find (fun r -> r.Contains point)
            let decoded = decoded @ [symbol.Data] // append to end

            let currentRange = symbol.EndPoint - symbol.StartPoint
            let nextPoint = (point - symbol.StartPoint) / currentRange
            
            iter decoded rangeTable nextPoint (length - 1)
    
    /// Decodes a point encoded using arithemtic encoding by using a range table and the length of the encoded data
    let decode (rangeTable:RangeDataPoint list) (point:bignum) (length:int) =
        iter [] rangeTable point length