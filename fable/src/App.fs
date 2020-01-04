module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Parser
open Browser.Types

// MODEL

type Model = int

type Msg =
| Increment
| Decrement


let drag (dragEvent:DragEvent) (dispatch:Msg->unit) =
  dragEvent.preventDefault ()
  printfn "Something has been dropped %A" dragEvent
  if not <| isNull dragEvent.dataTransfer then
    let file = dragEvent.dataTransfer.items.[0]
    file.getAsString(printfn "file: %A")
    file.getAsFile() |> printfn "asFile: %A"
    ()


let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =

  div [OnDrag (fun x-> x.preventDefault (); drag x dispatch); OnDragOver(fun event -> drag event dispatch; printfn "dragOver")]
      [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        h1 [] [str "Drop something here!"]
        div [ OnDrag (fun x-> x.preventDefault (); drag x dispatch) ] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run


["HelloWorld$INestedA.class"; "HelloWorld$INestedB.class"; "HelloWorld.class"; "HelloWorld$NestedA.class"; "HelloWorld$NestedB.class"; "module-info.class"]
|> List.iter(fun fileName ->
    { Data = [|0uy|]; Pos = 0 }
    |> parseHeader
    |> printfn "result for %s\n=========================\n%A" fileName)