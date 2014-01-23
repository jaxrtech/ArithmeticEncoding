open System
open System.Text
open Jaxrtech.ArithmeticEncoding

[<EntryPoint>]
let main argv =
    // Print out our text
    let text = "Hello world"
    printfn "Text = '%s'" text
    printfn "Length = %d" text.Length

    // Encode it
    let (table, encoded) = Encoder.encodeString text

    // Print out the range table
    printfn "Range Table:"
    for item in table do
        printfn "%c: (%f, %f)" (char item.Data) item.StartPoint item.EndPoint
    
    // Print out the actual value
    printfn "Encoded = %f" encoded 

    // Decode it back
    let decoded = Decoder.decodeString table encoded text.Length
    printfn "Decoded = '%s'" decoded

    printfn "Done"
    Console.ReadLine() |> ignore
    0
