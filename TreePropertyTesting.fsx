#r "DrawTrees/Library/net7.0/drawTrees.dll"
#r "nuget: FsCheck, 3.0.0-rc3"
#r "nuget: FsUnit, 6.0.0"

open DrawTrees.Trees
open FsCheck
open FsCheck.FSharp
open FsUnit

let rec mapper l1 l2 =
    match (l1, l2) with
    | [], qs -> qs
    | ps, [] -> ps
    | p :: ps, q :: qs -> [ List.append p q ] @ mapper ps qs


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






// Property 3

// i think the paper is incorrect about the equation for the property.
// note that, for the RHS, t is also reflected prior to calling design:


let rec reflect (Node(v, subtrees)) =
    Node(v, List.map reflect (List.rev subtrees))

let rec reflectpos (Node((v, x: float), subtrees)) =
    Node((v, -x), List.map reflectpos subtrees)








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





let t1 =
    Node(("A", 0.0), [ Node(("C", -1.0), []); Node(("D", 0.0), []); Node(("E", 1.0), []) ])

let t2 =
    Node(("A", 0.0), [ Node(("C", -2.0), []); Node(("D", 0.0), []); Node(("E", 2.0), []) ])





let treeGen myGen =
    let rec subtreeGen n =

        match n with
        | 0 -> Gen.map (fun v -> Node(v, [])) myGen
        | _ ->
            gen {
                let! children = Gen.choose (0, n / 2)

                let! value = myGen
                let! subtree = Gen.listOfLength children (subtreeGen (n / 2))
                return Node(value, subtree)
            }

    Gen.sized subtreeGen

let intTreeGen = ArbMap.defaults |> ArbMap.generate<int> |> treeGen

let rec subtrees e =
    let rec helper (st: 'a Tree list) =
        match st with
        | [] -> seq []
        | h :: tail ->
            seq {
                yield h
                yield! helper tail
            }

    match e with
    | Node(_, subtree) -> helper subtree

type GenericTreeGenerators =
    static member generic<'a>(contents: Arbitrary<'a>) =
        { new Arbitrary<'a Tree>() with
            override x.Generator = contents.Generator |> treeGen
            override x.Shrinker e = subtrees e }

type IntTreeGenerators =
    static member int() =
        { new Arbitrary<int Tree>() with
            override x.Generator = intTreeGen
            override x.Shrinker e = subtrees e }


let sampleProperty (tree: 'a Tree) =
    printfn "Generated tree: \n %A \n" tree
    true



let testProp1 t = (distanceProp (design t) 1)

let testProp2 t = centerProp (design t)

let testProp3 t =
    design t = reflect (reflectpos (design (reflect (t))))

let testProp4 t = identicalProp (design t)



let ensureValidTree t =
    match t with
    | Node(v, []) -> Node(v, [ (Gen.sample 1 (intTreeGen))[0] ])
    | _ -> t

let isValidTree t =
    match t with
    | Node(v, []) -> false
    | _ -> true

let rec ensureProperTree (t: int Tree) =
    match t with
    | Node(v, []) ->
        let leftT = (Gen.sample 1 (intTreeGen))[0]

        let rightT = (Gen.sample 1 (intTreeGen))[0]

        Node(v, [ leftT; rightT ])
    | Node(v, x :: []) -> ensureProperTree x
    | Node(_, _) -> t
// The tree neads atleast one node with 2 branches
let rec isProperTree t =
    match t with
    | Node(_, x :: y :: z) -> true
    | Node(_, x :: []) -> isProperTree x
    | Node(_, _) -> false

let rec hasMatchingSubtrees tree =
    let rec findSubtrees t map =
        match t with
        | Node(_, []) -> map
        | Node(_, children) ->
            match Map.tryFind (treeStructure 0 t) map with
            | Some(x) ->
                let v = Map.find (treeStructure 0 t) map
                let newMap = Map.add (treeStructure 0 t) (v + 1) map
                List.fold (fun acc elem -> findSubtrees elem acc) newMap children
            | None ->
                let newMap = Map.add (treeStructure 0 t) 1 map
                List.fold (fun acc elem -> findSubtrees elem acc) newMap children


    let map = (findSubtrees tree Map.empty)

    Map.exists (fun k v -> v > 1) map

let ensureMatchingSubtrees t =
    match hasMatchingSubtrees t with
    | true -> t
    | false ->
        let newT = ensureProperTree t
        let subtree = (Gen.sample 1 (intTreeGen))[0]

        match t with
        | Node(v, x) -> Node(v, subtree :: subtree :: x)

let rec treeSize t =
    let rec helper acc t =
        match t with
        | Node(_, []) -> acc + 1
        | Node(_, c) -> List.fold helper (acc + 1) c

    helper 0 t



let validTree =
    Arb.mapFilter ensureValidTree isValidTree (ArbMap.defaults.ArbFor<int Tree>())

let properTree =
    Arb.mapFilter ensureProperTree isProperTree (ArbMap.defaults.ArbFor<int Tree>())

let matchingSubtrees =
    Arb.mapFilter ensureMatchingSubtrees hasMatchingSubtrees (ArbMap.defaults.ArbFor<int Tree>())



let prop1Classify x =
    (testProp1 x
     |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
     |> Prop.classify (not (isValidTree x)) "false precondition"
     |> Prop.classify ((isValidTree x)) "valid tree"
     |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
     |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop2Classify x =
    (testProp2 x
     |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
     |> Prop.classify (not (isValidTree x)) "false precondition"
     |> Prop.classify ((isValidTree x)) "valid tree"
     |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
     |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop3Classify x =
    (testProp3 x
     |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
     |> Prop.classify (not (isValidTree x)) "false precondition"
     |> Prop.classify ((isValidTree x)) "valid tree"
     |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
     |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop4Classify x =
    (testProp4 x
     |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
     |> Prop.classify (not (isValidTree x)) "false precondition"
     |> Prop.classify ((isValidTree x)) "valid tree"
     |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
     |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop1WithArbClassify =
    Prop.forAll properTree (fun x ->
        testProp1 x
        |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
        |> Prop.classify (not (isValidTree x)) "false precondition"
        |> Prop.classify ((isValidTree x)) "valid tree"
        |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
        |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop2WithArbClassify =
    Prop.forAll validTree (fun x ->
        testProp2 x
        |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
        |> Prop.classify (not (isValidTree x)) "false precondition"
        |> Prop.classify ((isValidTree x)) "valid tree"
        |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
        |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop3WithArbClassify =
    Prop.forAll validTree (fun x ->
        testProp3 x
        |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
        |> Prop.classify (not (isValidTree x)) "false precondition"
        |> Prop.classify ((isValidTree x)) "valid tree"
        |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
        |> Prop.classify ((isProperTree x)) "is a proper tree")

let prop4WithArbClassify =
    Prop.forAll matchingSubtrees (fun x ->
        testProp4 x
        |> Prop.classify ((hasMatchingSubtrees x)) "has matching subtree structure"
        |> Prop.classify (not (isValidTree x)) "false precondition"
        |> Prop.classify ((isValidTree x)) "valid tree"
        |> Prop.classify ((treeSize x) > 50) "has more than 50 nodes"
        |> Prop.classify ((isProperTree x)) "is a proper tree")

type ListPropertiesClassify =
    static member ``prop1``(t: int Tree) = prop1Classify t
    static member ``prop2``(t: int Tree) = prop2Classify t
    static member ``prop3``(t: int Tree) = prop3Classify t
    static member ``prop4``(t: int Tree) = prop4Classify t


type ListPropertiesWithArbClassify =
    static member ``prop1WithArbClassify``() = prop1WithArbClassify
    static member ``prop2WithArbClassify``() = prop2WithArbClassify
    static member ``prop3WithArbClassify``() = prop3WithArbClassify
    static member ``prop4WithArbClassify``() = prop4WithArbClassify

let config = Config.Quick.WithArbitrary([ typeof<IntTreeGenerators> ])

Check.QuickAll(typeof<ListPropertiesClassify>)
Check.All(config, typeof<ListPropertiesWithArbClassify>)

let wrongTree =
    Node(
        ("A", 0.0),
        [ Node(("B", -0.5), [ Node(("E", -0.5), []) ])
          Node(("C", -1), [ Node(("F", 0), []) ]) ]
    )

let badReflection =
    Node(
        ("A", 0.0),
        [ Node(("B", -0.5), [ Node(("E", 0), []) ])
          Node(("C", -1), [ Node(("F", -0.5), []) ]) ]
    )


(distanceProp wrongTree 1) |> should equal false
(centerProp wrongTree) |> should equal false
(wrongTree = reflect (reflectpos (badReflection))) |> should equal false
(identicalProp wrongTree) |> should equal false
