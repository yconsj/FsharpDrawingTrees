#r "DrawTrees/Library/drawTrees.dll"
#r "nuget: FsCheck"

open DrawingTrees
open FsCheck






let l1 = [ [ 1; 2 ] ]
let l2 = [ [ 4 ]; [ 3 ] ]

let rec mapper l1 l2 =
    match (l1, l2) with
    | [], qs -> qs
    | ps, [] -> ps
    | p :: ps, q :: qs -> [ List.append p q ] @ mapper ps qs
    | _ -> []


printf "%A" (mapper l1 l2)


let rec treeToList (t: Tree<'a * float>) : List<List<'a>> =
    match t with
    | Node((a, f), []) -> [ [ a ] ]
    | Node((a, f), d) -> [ [ a ] ] @ List.fold (fun acc elem -> mapper acc (treeToList elem)) [] d


let rec relativeDistances (t: Tree<'a * float>) : List<List<float>> =
    match t with
    | Node((a, f), []) -> [ [ f ] ]
    | Node((a, f), d) -> [ [ f ] ] @ List.fold (fun acc elem -> mapper acc (relativeDistances elem)) [] d

let absoluteDistances (tree: Tree<'a * float>) : List<List<float>> =
    let rec absoluteDistances' (t: Tree<'a * float>) v : List<List<float>> =
        match t with
        | Node((a, f), []) -> [ [ f + v ] ]
        | Node((a, f), d) ->
            [ [ f + v ] ]
            @ List.fold (fun acc elem -> mapper acc (absoluteDistances' elem (f + v))) [] d

    absoluteDistances' tree 0

let tree =
    Node(
        "A",
        [ Node("B", [ Node("C", []); Node("D", []); Node("E", []) ])
          Node("F", [ Node("G", [ Node("H", []); Node("I", []); Node("J", []) ]) ]) ]
    )

let dTree = design tree
printf "%A \n" tree
printf "%A \n " dTree
printf "%A\n" (treeToList dTree)
printf "%A\n" (relativeDistances dTree)
printf "%A\n" (absoluteDistances dTree)

let rec compareDistances l v =
    match l with
    | [] -> true
    | x :: y :: t when abs (x - y) > v -> false
    | x :: y :: t when abs (x - y) <= v -> compareDistances (y :: t) v

let distanceProp tree v =
    let rec compareDistances l =
        match l with
        | [] -> true
        | x :: y :: t when abs (x - y) >= v -> compareDistances (y :: t)
        | x :: y :: t when abs (x - y) < v -> false
        | _ -> true

    let rec distanceProp' d =
        match d with
        | [] -> true
        | h :: t ->
            let s = List.sort h
            if compareDistances s then distanceProp' t else false

    distanceProp' (absoluteDistances tree)

// Property 2
// check sum of children relative distances is zero
let sumCheck tl =
    List.fold (fun acc (Node((a, v), l)) -> v + acc) 0.0 tl

let rec centerProp (t: ('a * float) Tree) : bool =
    match t with
    | Node((a, v), []) -> true
    | Node((a, v), st) -> if sumCheck st <> 0.0 then false else helper st

and helper tl =
    match tl with
    | x :: y -> centerProp x || helper tl
    | [] -> true

printf "%A\n" (centerProp dTree)


let test t = distanceProp (design t) 1
let _ = Check.Quick test
let _ = Check.Verbose test
// Property 3
