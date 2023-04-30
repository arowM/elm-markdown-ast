module MarkdownAstSuite exposing (suite)

import Expect
import MarkdownAst as Ast
import Sample exposing (myMarkdown)
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

* List Item 1😈
    1. Child item
* List Item 2

    Child paragraph. \\[Link like\\]\\(./src\\) in plain text. 1. foo  
    2\\. bar

    1\\. dummy ordered list item

    ```text
    type builder parent elem =
        ...
        ...
    ```

    ```elm
    type Builder parent elem =
        ...
        ...
    ```
* **List** **Item** 3

    > [Child paragraph](./bar "title here").
    >
    > ```elm
    > type Builder parent elem =
    >     ...
    >     ...
    > ```

    ![dummy image](./foo)
* List Item 4"""
