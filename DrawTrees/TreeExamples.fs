// @author Simon Janum
// @author August Valentin
// @date 13/6/2024
namespace DrawTrees

module TreeExamples =
    open Trees

    let largeTree =
        Node(
            "A",
            [ Node(
                  "B",
                  [ Node(
                        "C",
                        [ Node("D", [])
                          Node(
                              "E",
                              [ Node(
                                    "F",
                                    [ Node("G", [])
                                      Node("H", [ Node("I", []); Node("J", []); Node("K", []); Node("L", []) ])
                                      Node("M", [])
                                      Node("N", [ Node("O", []) ]) ]
                                ) ]
                          ) ]
                    )
                    Node("P", [ Node("Q", []); Node("R", []) ]) ]
              )
              Node(
                  "S",
                  [ Node(
                        "T",
                        [ Node("U", [])
                          Node("V", [])
                          Node(
                              "W",
                              [ Node("X", [ Node("Y", []) ])
                                Node("Z", [ Node("a", []); Node("b", []); Node("c", []); Node("d", []) ]) ]
                          ) ]
                    )
                    Node(
                        "e",
                        [ Node("f", [ Node("g", []) ])
                          Node("h", [])
                          Node("i", [ Node("j", [ Node("k", []); Node("l", []); Node("m", []); Node("n", []) ]) ]) ]
                    ) ]
              )
              Node(
                  "o",
                  [ Node(
                        "p",
                        [ Node("q", [ Node("r", []); Node("s", []); Node("t", []); Node("u", []) ])
                          Node(
                              "v",
                              [ Node("w", [])
                                Node("x", [ Node("y", []); Node("z", []) ])
                                Node("0", [])
                                Node("1", []) ]
                          )
                          Node("2", []) ]
                    ) ]
              ) ]
        )

    let simpleTree =
        Node(
            "A",
            [ Node(
                  "B",
                  [ Node(
                        "C",
                        [ Node("D", [])
                          Node(
                              "E",
                              [ Node(
                                    "F",
                                    [ Node("G", [])
                                      Node("H", [ Node("I", []); Node("J", []); Node("K", []); Node("L", []) ]) ]
                                ) ]
                          ) ]
                    ) ]
              ) ]
        )

    let codeStructureTree =
        Node(
            "ProjectFolder/",
            [ Node(
                  "DrawTrees/",
                  [ Node("bin/", [ Node("...", []) ])
                    Node(
                        "Library/",
                        [ Node(
                              "net7.0/",
                              [ Node("DrawTrees.deps.json", [])
                                Node("DrawTrees.dll", [])
                                Node("DrawTrees.pdb", [])
                                Node("DrawTrees.xml", []) ]
                          ) ]
                    )
                    Node(
                        "obj/",
                        [ Node("Debug/", [ Node("...", []) ])
                          Node("DrawTrees.dll", [])
                          Node("...", []) ]
                    )
                    Node("DrawTrees.fsproj", [])
                    Node("Tree.fs", [])
                    Node("TreePlotter.fs", [])
                    Node("TreeExamples.fs", []) ]
              )
              Node("TreePlotter.fsx", [])
              Node("TreePropertyTesting.fsx", []) ]
        )
