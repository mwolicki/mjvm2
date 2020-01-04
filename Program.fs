﻿open Parser

["HelloWorld$INestedA.class"; "HelloWorld$INestedB.class"; "HelloWorld.class"; "HelloWorld$NestedA.class"; "HelloWorld$NestedB.class"; "module-info.class"]
|> List.iter(fun fileName ->
    "/Users/kevin/_projects/java/" + fileName
    |> readFile
    |> parseHeader
    |> printfn "result for %s\n=========================\n%A" fileName)