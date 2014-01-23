[<AutoOpen>]
module Jaxrtech.ArithmeticEncoding.CommonTypes

/// A range bounded at two points
[<NoComparison>]
type ValueRange(startPoint: bignum, endPoint: bignum) =
    member this.StartPoint = startPoint
    member this.EndPoint = endPoint

    /// Returns the range between the two points
    member this.Range =
        this.EndPoint - this.StartPoint

    /// Returns the middle point between the two points
    member this.Middle =
        (this.EndPoint + this.StartPoint) / 2N

    /// Returns if the range contains a value exclusively
    member this.Contains value =
        this.StartPoint < value && value < this.EndPoint

    /// Returns if the range contains another range exclusively
    member this.Contains (range:ValueRange) =
        this.StartPoint < range.StartPoint && range.EndPoint < this.EndPoint

    /// Returns if the range contains a value inclusively
    member this.ContainsOrIsOn value =
        this.StartPoint <= value && value <= this.EndPoint
    
    /// Returns if the range contains a range inclusively
    member this.ContainsOrIsOn (range:ValueRange) =
        this.StartPoint <= range.StartPoint && range.EndPoint <= this.EndPoint

    /// Returns if the range is contained within another range exclusively
    member this.IsWithin (range:ValueRange) =
        range.Contains this

    /// Returns if the range is contained within another range inclusively
    member this.IsWithinOrIsOn (range:ValueRange) =
        range.ContainsOrIsOn this

    override this.Equals (other:obj) =
        match other with
        | :? ValueRange as r -> this.StartPoint = r.StartPoint && this.EndPoint = r.EndPoint
        | _ -> false

    override this.GetHashCode () =
        hash (this.StartPoint, this.EndPoint)

module ValueRanges =
    /// Returns the projection for the start point of a ValueRange
    let startPointProjection<'T when 'T :> ValueRange> =
        fun (range:'T) -> range.StartPoint

    /// Returns the projection for the end point of a ValueRange
    let endPointProjection<'T when 'T :> ValueRange> =
        fun (range:'T) -> range.EndPoint

/// Data point holding a percentage range
type PercentageDataPoint = { Data: byte; Percent: bignum }

/// Data point represented by a range
type RangeDataPoint(data: byte, range: ValueRange) =
    inherit ValueRange(range.StartPoint, range.EndPoint)
    member this.Data = data

/// Types of data points
type DataPoint =
     | Percentage of PercentageDataPoint
     | Range of RangeDataPoint