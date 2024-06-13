#r "nuget: Plotly.NET, 4.0.0"
#r "DrawTrees/Library/net7.0/drawTrees.dll"

open Plotly.NET
open DrawTrees.Trees
open DrawTrees.TreeExamples
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
                MultiTextPosition = [ StyleParam.TextPosition.MiddleCenter ],
                ShowLegend = false

            )
            |> Chart.withMarkerStyle (Opacity = 0.0) // Make the marker invisible
        )


    Seq.append seq pSeq

let addLine (x1, y1) (x2, y2) seq =
    let lSeq =
        Seq.singleton (Chart.Line([ x1; x2 ], [ y1; y2 ], LineColor = Color.fromString "green", ShowLegend = false))
    Seq.append seq lSeq

let formatter a sep =

    let s = string a
    let l = (Seq.toList s)
    let charL = List.splitInto (max (l.Length / sep) 1) l
    List.map (fun elem -> System.String(elem |> List.toArray)) charL

let drawTreeV1 t =
    let rec drawTreeChildren tree parent seq =
        let (parentX, parentY, parentOffset) = parent

        match tree with
        | Node((a, v), []) ->
            let formatted = (formatter a 5)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.075

            let temp =
                addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq

            addLine (parentX, parentY - parentOffset) (parentX + v, parentY - 1.0 + offset) temp


        | Node((a, v), st) ->
            let formatted = (formatter a 5)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.075

            let temp =
                addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq

            let seq =
                addLine (parentX, parentY - parentOffset) (parentX + v, parentY - 1.0 + offset) temp
            List.fold
                (fun acc elem -> Seq.append acc (drawTreeChildren elem (parentX + v, parentY - 1.0, offset)  Seq.empty))
                seq
                st

    // Starting at root - shouldnt draw a line
    match t with
    | Node((a, v), []) ->
        let formatted = (formatter a 5)
        addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
    | Node((a, v), st) ->
        let formatted = (formatter a 5)

        let offset =
            if (float formatted.Length) = 1 then
                0.1
            else
                (float formatted.Length) * 0.075

        let seq = addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
        List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (0.0, 0.0, offset) Seq.empty)) seq st

    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show


let drawTreeV2 t =
    let rec drawTreeChildren tree parent seq =
        let (parentX, parentY) = parent

        match tree with
        | Node((a, v), []) ->
            let formatted = (formatter a 5)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.075

            let seq =
                addLine (parentX + v, parentY - 1.0 + offset) (parentX + v, parentY + 0.5 - 1.0) seq

            addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq


        | Node((a, v), st) ->
            let formatted = (formatter a 5)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.075


            let seq =
                addLine (parentX + v, parentY - 1.0 + offset) (parentX + v, parentY + 0.5 - 1.0) seq // line up

            let seq =
                addLine (parentX + v, parentY - 1.0 - offset) (parentX + v, parentY - 0.5 - 1.0) seq // Down line

            let (Node((_, vfirst), _)) = st.Head
            let (Node((_, vlast), _)) = (List.rev st).Head

            let seq =
                addLine (parentX + v + vfirst, parentY - 1.5) (parentX + v + vlast, parentY - 1.5) seq



            let seq = addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq

            List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (parentX + v, parentY - 1.0) Seq.empty)) seq st

    // Starting at root - shouldnt draw a line
    match t with
    | Node((a, v), []) -> addPoint (0.0, 0.0) a Seq.empty
    | Node((a, v), st) ->
        let formatted = (formatter a 5)

        let offset =
            if (float formatted.Length) = 1 then
                0.1
            else
                (float formatted.Length) * 0.075

        let seq = addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
        let seq = addLine (0.0, 0.0 - offset) (0.0, - 0.5) seq
        let (Node((_, vfirst), _)) = st.Head
        let (Node((_, vlast), _)) = (List.rev st).Head
        let seq = addLine (vfirst, -0.5) (vlast, -0.5) seq
        
        List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (0.0, 0.0) Seq.empty)) seq st

    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.withSize(900.0,700.0)
    |> Chart.show




let renderTree b t =
    if b then drawTreeV1 t else drawTreeV2 t

//printf "%A" (design codeStructureTree)
renderTree false (design codeStructureTree)