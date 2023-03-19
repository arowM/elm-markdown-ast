module MarkdownAstSuite exposing (suite)

import Expect
import MarkdownAst as Ast
import Test exposing (..)


suite : Test
suite =
    test "render sample markdown as expected." <|
        \_ ->
            myMarkdown
                |> Ast.render
                |> Expect.equal """# Markdown AST

Markdown AST represents *Markdown* AST.

## Section

* List Item 1
    1. Child item
* List Item 2

    Child paragraph. \\[Link like\\]\\(./src\\) in plain text. 1. foo  
    2\\. bar

    1\\. dummy ordered list item

    ```elm
    type Builder parent elem =
        ...
        ...
    ```
* List Item 3

    > [Child paragraph](./bar "title here").
    >
    > ```elm
    > type Builder parent elem =
    >     ...
    >     ...
    > ```

    ![dummy image](./foo)
* List Item 4"""


myMarkdown : Ast.Section
myMarkdown =
    Ast.Section
        { title = "Markdown AST"
        , body =
            [ Ast.ParagraphBlock
                [ Ast.PlainText "Markdown  \n AST represents  "
                , Ast.Emphasis " \n\tMarkdown \n\n"
                , Ast.PlainText " AST."
                ]
            ]
        , children =
            [ Ast.Section
                { title = "Section"
                , body =
                    [ Ast.ListBlock
                        { ordered = False
                        , items =
                            [ { content =
                                [ Ast.PlainText "List Item 1"
                                ]
                              , children =
                                  [ Ast.ListBlock
                                    { ordered = True
                                    , items =
                                        [ { content =
                                            [ Ast.PlainText "Child item"
                                            ]
                                          , children = []
                                    }
                                        ]
                                    }
                                  ]
                                }
                            , { content =
                                [ Ast.PlainText "List Item 2"
                                ]
                              , children =
                                  [ Ast.ParagraphBlock
                                    [ Ast.PlainText "Child paragraph."
                                    , Ast.PlainText " [Link like](./src) in plain text.\n1. foo"
                                    , Ast.LineBreak
                                    , Ast.PlainText "2. bar"
                                    ]
                                  , Ast.ParagraphBlock
                                    [ Ast.PlainText "1. dummy ordered list item"
                                    ]
                                  , Ast.CodeBlock
                                    """elm
                                    type Builder parent elem =
                                        ...
                                        ...
                                    """
                                  ]
                              }
                            , { content =
                                [ Ast.PlainText "List Item 3"
                                ]
                              , children =
                                  [ Ast.QuoteBlock
                                    [ Ast.ParagraphBlock
                                      [ Ast.Link
                                        { text = "\tChild paragraph\n\n"
                                        , href = "./bar"
                                        , title = Just "\n  title   \there "
                                        }
                                      , Ast.PlainText "."
                                      ]
                                    , Ast.CodeBlock
                                      """elm
                                      type Builder parent elem =
                                          ...
                                          ...
                                      """
                                    ]
                                  , Ast.ParagraphBlock
                                    [ Ast.Image
                                      { src = "./foo"
                                      , alt = "  dummy  image\n"
                                      , title = Nothing
                                      }
                                    ]
                                  ]
                              }
                            , { content =
                                [ Ast.PlainText "List Item 4"
                                ]
                              , children = []
                              }
                            ]
                        }
                    ]
                , children = []
                }
            ]
        }
