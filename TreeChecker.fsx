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
    | x :: y :: t when abs (x - y) <= v -> compareDistances (y :: t) v
    | x :: y :: _ when abs (x - y) > v -> false
    | _ -> true

let distanceProp tree v =
    let rec compareDistances l =
        match l with
        | [] -> true
        | x :: y :: t when abs (x - y) >= v -> compareDistances (y :: t)
        | x :: y :: _ when abs (x - y) < v -> false
        | _ -> true

    let rec distanceProp' d =
        match d with
        | [] -> true
        | h :: t ->
            let s = List.sort h
            if compareDistances s then distanceProp' t else false

    distanceProp' (absoluteDistances tree)

// Property 2
// check sum of first and last children distances is zero
let rec centerProp t =
    match t with
    | Node((a, v), []) -> true
    | Node((a, v), st) ->
        let (Node((x1, y1), st1)) = List.head st
        let (Node((x2, y2), st2)) = List.last st

        if (y1 + y2) <> 0.0 then
            false
        else
            List.fold (fun acc elem -> acc && centerProp elem) true st

printf "Prop 2: %A\n" (centerProp dTree)





// Property 3
// left side of subtree = right side of subtree, in negation

let rec symmetryProp (t: ('a * float) Tree) : bool =
    match t with
    | Node((_, v), []) -> true
    | Node((_, v), (Node((_, v1), _)) :: []) -> if v1 <> 0.0 then false else true
    | Node((_, v), st) ->
        let (Node((_, y1), _)) :: l1 = st
        let (Node((_, y2), _)) :: l2 = List.rev l1

        if abs (y1) <> abs (y2) then
            false
        else
            List.fold (fun acc elem -> symmetryProp elem) true st

printf "%A\n" (symmetryProp dTree)


// Prop 4





let rec reflect (Node(v, subtrees)) =
    Node(v, List.map reflect (List.rev subtrees))

let rec reflectpos (Node((v, x: float), subtrees)) =
    Node((v, -x), List.map reflectpos subtrees)



let testProp1 t = distanceProp (design t) 1

let testProp2 t = centerProp (design t)
let testProp3 t = symmetryProp (design t)
let _ = Check.Quick testProp1
let _ = Check.Quick testProp2
let _ = Check.Quick testProp3


let temp =
    Node(
        ("A", 0.0),
        [ Node(("B", -1.0), [ Node(("C", 0.0), []); Node(("D", 0.0), []); Node(("E", 0.0), []) ])
          Node(("F", 0.0), [])
          Node(("G", 1.0), [ Node(("H", 0.0), []); Node(("I", 0.0), []); Node(("J", 0.0), []) ]) ]
    )

printf "%A\n" (temp)
printf "%A\n" (reflect (reflectpos (temp)))

let test t = distanceProp (design t) 1
let _ = Check.Quick test
//let _ = Check.Verbose test
// Property 3


// i think the paper is incorrect about the equation for the property.
// note that, for the RHS, t is also reflected prior to calling design:
let mirrorProp t =
    design t = reflect (reflectpos (design (reflect (t))))

let _ = Check.Quick mirrorProp
