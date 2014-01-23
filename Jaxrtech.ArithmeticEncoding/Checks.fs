module Jaxrtech.ArithmeticEncoding.Checks

open System
open Jaxrtech.ArithmeticEncoding.Converters

module ValueRanges =
    /// Checks that a ValueRange list is within the valid encoding range
    let check<'T when 'T :> ValueRange> (ranges:'T list) =
        assert (ranges.Length > 0)
        let ranges = ranges |> List.sortBy ValueRanges.startPointProjection

        let startPoint = ranges.Head.StartPoint
        let endPoint = let p = ranges.[ranges.Length - 1].EndPoint
                       Math.Round(p, 10) // account for floating point error
        assert (startPoint = entireRange.StartPoint)
        assert (endPoint = entireRange.EndPoint)

        ranges