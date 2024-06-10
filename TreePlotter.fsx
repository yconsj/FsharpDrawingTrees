#r "nuget: Plotly.NET, 4.0.0"
#r "DrawTrees/Library/drawTrees.dll"
#r "nuget: FsCheck"

open Plotly.NET
open System
open DrawingTrees
open FsCheck
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling


let mirroredXAxis =
    LinearAxis.init (
        //        Title = Title.init(Text="Mirrored axis"),
        ShowLine = false,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = false
    //        Ticks = StyleParam.TickOptions.Inside
    )

let mirroredYAxis =
    LinearAxis.init (
        //        Title = Title.init(Text="Log axis"),
        //        AxisType = StyleParam.AxisType.Log,
        ShowLine = false,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = false
    )

let test () =
    [ Chart.Point(
          [ (0, 4.5) ],
          MultiText = [ "A Point" ],
          MultiTextPosition = [ StyleParam.TextPosition.TopCenter ],
          ShowLegend = false
      )
      Chart.Line([ 0.0; 0.0 ], [ 4.5; 0 ], LineColor = Color.fromString "green", ShowLegend = false)


      Chart.Point(
          [ (0, 0) ],
          MultiText = [ "B Point" ],
          MultiTextPosition = [ StyleParam.TextPosition.TopCenter ],
          ShowLegend = false
      )

      ]
    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show


let addPoint p name seq =
    let pSeq =
        Seq.singleton (
            Chart.Point(
                [ p ],
                MultiText = [ name ],
                MultiTextPosition = [ StyleParam.TextPosition.TopCenter ],
                ShowLegend = false
            )
        )

    Seq.append seq pSeq

let addLine (x1, y1) (x2, y2) seq =
    let lSeq =
        Seq.singleton (Chart.Line([ x1; x2 ], [ y1; y2 ], LineColor = Color.fromString "green", ShowLegend = false))

    Seq.append seq lSeq


let drawTree t =
    let rec drawTreeChildren tree parent seq =
        let (parentX, parentY) = parent

        match tree with
        | Node((a, v), []) ->
            let temp = addPoint (parentX + v, parentY - 1.0) a seq
            addLine (parentX, parentY - 0.1) (parentX + v, parentY - 1.0 + 0.1) temp


        | Node((a, v), st) ->
            let temp = addPoint (parentX + v, parentY - 1.0) a seq
            let seq = addLine (parentX, parentY - 0.1) (parentX + v, parentY - 1.0 + 0.1) temp

            List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (parentX + v, parentY - 1.0) seq)) seq st

    // Starting at root - shouldnt draw a line
    match t with
    | Node((a, v), []) -> addPoint (0.0, 0.0) a Seq.empty
    | Node((a, v), st) ->
        let seq = addPoint (0.0, 0.0) a Seq.empty
        List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (0.0, 0.0) seq)) seq st

    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show



let tree =
    Node(
        "A",
        [ Node("B", [ Node("C", []); Node("D", []); Node("E", []) ])
          Node("F", [ Node("G", [ Node("H", []); Node("I", []); Node("J", []) ]) ]) ]
    )


// Implement generators
let rec treeGen<'a> (genA: Gen<'a>) (size: int) : Gen<'a Tree> =
    if size = 0 then
        gen {
            let! value = genA
            return Node(value, [])
        }
    else
        let smallerTreeGen = treeGen genA (size / 2)

        let nodeGen =
            gen {
                let! value = genA
                let! childrenCount = Gen.choose (0, size)
                let! children = Gen.listOfLength childrenCount smallerTreeGen
                return Node(value, children)
            }

        nodeGen

let treeGenSized<'a> (genA: Gen<'a>) : Gen<'a Tree> = Gen.sized (treeGen genA)

// Generate a single tree example
let generateSampleTree<'a> (genA: Gen<'a>) =
    let sampleTree = Gen.sample 5 1 (treeGenSized genA) |> List.head
    sampleTree


let intTreeGen = treeGenSized Arb.generate<int>
let sampleTree = generateSampleTree Arb.generate<int>


printf "%A \n" (sampleTree)
drawTree (design sampleTree)
