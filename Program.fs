open Parser


let x =  readFile "/Users/kevin/_projects/java/HelloWorld.class" |> (parseHeader)
printfn "result = %A" x 