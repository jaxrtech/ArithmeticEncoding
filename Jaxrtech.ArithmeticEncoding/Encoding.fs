[<AutoOpen>]
module Jaxrtech.ArithmeticEncoding.Encoding

open System
open System.Text
open Jaxrtech.ArithmeticEncoding.Converters
open Jaxrtech.ArithmeticEncoding.Checks
    
/// Encodes a string into bytes using UTF8
let encodeString (text:string) = Encoding.UTF8.GetBytes(text) |> Array.toList

let decodeString (buffer:byte[]) = Encoding.UTF8.GetString(buffer)

module Encoder =
    // TODO: Add an option to disable checks. Have the `ValueRanges.check` just be passed with and id map or something

    /// Encodes a buffer of bytes into a tuple of the percentage table and the represented range of the enocded data
    let encodeBuffer (buffer:byte list) =
        let table = buffer
                    |> Data.toPercentages
                    |> PercentageDataPoints.toRanges

        let encoded =
            let range = table
                        |> ValueRanges.check
                        |> RangeDataPoints.encode buffer
            range.Middle
        
        (table, encoded)

    /// Encodes a string into a tuple of the percentage table and the represented range of the enocded data
    let encodeString text =
        text
        |> encodeString
        |> encodeBuffer

module Decoder =
    /// Decodes a point encoded using arithmetic encodeding from range table and the length of the encoded data
    let decodeBuffer (rangeTable:RangeDataPoint list) (point:bignum) (length:int) =
        Point.decode rangeTable point length

    let decodeString (rangeTable:RangeDataPoint list) (point:bignum) (length:int) =
        decodeBuffer rangeTable point length
        |> List.toArray
        |> decodeString