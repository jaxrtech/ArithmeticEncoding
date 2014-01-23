open System
open System.Text
open Jaxrtech.ArithmeticEncoding

type Input =
     | Data of byte[]
     | Text of string

let runEncodingTest (input:Input) =
    let printResults (table:RangeDataPoint list) (encoded:bignum) =
        // Print out the range table
        printfn "Range Table:"
        for item in table do
            printfn "%c: (%s, %s)" (char item.Data) (item.StartPoint.ToString()) (item.EndPoint.ToString())
    
        // Print out the actual value
        printfn "Encoded = %s" (encoded.ToString())
        printfn "sizeof(Numberator)  = %d bytes" (encoded.Numerator.ToByteArray().Length)
        printfn "sizeof(Denominator) = %d bytes" (encoded.Denominator.ToByteArray().Length)

    match input with
    | Text text ->
        // Print out our text
        printfn "Text = '%s'" (text.ToString())
        printfn "Length = %d" text.Length

        // Encode it
        let (table:RangeDataPoint list, encoded:bignum) = Encoder.encodeString text

        printResults table encoded

        // Decode it back
        let decoded = Decoder.decodeString table encoded text.Length
        printfn "Decoded = '%s'" decoded
    
        (table, encoded)
    | Data data ->
        // Print out our data
        printfn "Data = %A" data
        printfn "Length = %d" data.Length

        // Encode it
        let (table:RangeDataPoint list, encoded:bignum) = data |> Array.toList |> Encoder.encodeBuffer 

        printResults table encoded

        // Decode it back
        let decoded = Decoder.decodeBuffer table encoded data.Length
        printfn "Decoded = %A" decoded
    
        (table, encoded)

[<EntryPoint>]
let main argv =
    let (table, encoded) = runEncodingTest (Text "Hello")
    runEncodingTest (Data (encoded.Numerator.ToByteArray())) |> ignore

    printfn "Done"
    Console.ReadLine() |> ignore
    0