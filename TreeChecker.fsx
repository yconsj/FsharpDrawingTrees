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

// i think the paper is incorrect about the equation for the property.
// note that, for the RHS, t is also reflected prior to calling design:


let rec reflect (Node(v, subtrees)) =
    Node(v, List.map reflect (List.rev subtrees))

let rec reflectpos (Node((v, x: float), subtrees)) =
    Node((v, -x), List.map reflectpos subtrees)



let testProp1 t = distanceProp (design t) 1

let testProp2 t = centerProp (design t)

let testProp3 t =
    design t = reflect (reflectpos (design (reflect (t))))


let _ = Check.Quick testProp1
let _ = Check.Quick testProp2
let _ = Check.Quick testProp3

let wrongTree =
    Node(
        ("A", 0.0),
        [ Node(("B", -1.0), [ Node(("C", 0.0), []); Node(("D", 0.0), []); Node(("E", 0.0), []) ])
          Node(("F", 1), [ Node(("G", 0), [ Node(("H", -2.0), []); Node(("I", 0.0), []); Node(("J", 2.0), []) ]) ]) ]
    )

printf "%A\n" (wrongTree)
printf "%A\n" (design wrongTree = reflect (reflectpos (design wrongTree)))


printf "%A\n" (relativeDistances (design tree))







let rec compareTrees t1 t2 =
    match (t1, t2) with
    | Node((_, v1), []), Node((_, v2), []) when v1 <> v2 -> false
    | Node((_, v1), []), Node((_, v2), []) when v1 = v2 -> true
    | Node((_, v1), st1), Node((_, v2), st2) when v1 <> v2 -> false
    | Node((_, v1), st1), Node((_, v2), st2) when List.length st1 = List.length st2 ->
        List.fold2 (fun acc elem1 elem2 -> acc && compareTrees elem1 elem2) true st1 st2
    | _, _ -> false

let rec treeStructure layer t =
    match t with
    | Node(_, children) ->
        let newChildren = List.map (treeStructure (layer + 1)) children
        Node(layer, newChildren)

let rec removeTreeName t =
    match t with
    | Node((_, value), children) ->
        let newChildren = List.map removeTreeName children
        Node(value, newChildren)


let identicalProp tree =
    let rec findSubtrees t map =
        match t with
        | Node(_, []) -> map
        | Node(_, children) ->
            let newMap = Map.add t (treeStructure 0 t) map
            List.fold (fun acc elem -> findSubtrees elem acc) newMap children

    let map = (findSubtrees (removeTreeName tree) Map.empty)

    Map.forall
        (fun (Node(_, key1)) elem1 -> Map.forall (fun (Node(_, key2)) elem2 -> not (key1 <> key2 && elem1 = elem2)) map)
        map



let wrongIdenticalTree =
    Node(
        ("A", 0.0),
        [ Node(("B", -1.0), [ Node(("C", -0.5), []); Node(("D", 0.0), []); Node(("E", 0.5), []) ])
          Node(("F", 1.0), [ Node(("G", 0.0), [ Node(("H", -1.0), []); Node(("I", 0.0), []); Node(("J", 1.0), []) ]) ]) ]
    )

let identicalTree =
    Node(
        ("A", 0.0),
        [ Node(("B", -1.0), [ Node(("C", -0.5), []); Node(("D", 0.0), []); Node(("E", 0.5), []) ])
          Node(("F", 1.0), [ Node(("G", 0.0), [ Node(("H", -0.5), []); Node(("I", 0.0), []); Node(("J", 0.5), []) ]) ]) ]
    )



let t1 =
    Node(("A", 0.0), [ Node(("C", -1.0), []); Node(("D", 0.0), []); Node(("E", 1.0), []) ])

let t2 =
    Node(("A", 0.0), [ Node(("C", -2.0), []); Node(("D", 0.0), []); Node(("E", 2.0), []) ])


printf "%A\n" (identicalProp (wrongIdenticalTree))

printf "%A\n" (identicalProp (identicalTree))

printf "%A\n" (identicalProp t1)


let testProp4 t = identicalProp (design t)
let _ = Check.Quick testProp3
