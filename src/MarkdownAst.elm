module MarkdownAst exposing
    ( Section(..)
    , BlockElement(..)
    , InlineElement(..)
    , ListItem
    , render
    , preview
    )

{-| This module provides abstruct syntax tree for markdown.


# Structure

@docs Section
@docs BlockElement
@docs InlineElement
@docs ListItem


# Render

@docs render
@docs preview

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Internal exposing (headTicks, inlineTicks, trimIndent)
import Url


{-| Represents markdown section.

    # Title for Root Section

    Paragraph text in root section body.

    Another paragraph text in root section body.

    * List item in root section body
    * Another list item in root section body

    ## Title for Child Section

    Paragraph text in child section body.

    ```json
    {
      "message": "Code block in child section body."
    }
    ```

    ## Title for Another Child Section

    Paragraph text in another child section body.

    1. Ordered list item in another child section body
    1. Another ordered list item in another child section body

-}
type Section
    = Section
        { title : String
        , body : List BlockElement
        , children : List Section
        }


{-| -}
type BlockElement
    = ParagraphBlock (List InlineElement)
    | ListBlock
        { ordered : Bool
        , items : List ListItem
        }
    | CodeBlock String
    | QuoteBlock (List BlockElement)


{-| -}
type InlineElement
    = PlainText String
    | Link
        { href : String
        , text : String
        , title : Maybe String
        }
    | Image
        { src : String
        , alt : String
        , title : Maybe String
        }
    | InlineCode String
    | Emphasis String
    | StrongEmphasis String
    | Strikethrough String
    | LineBreak


{-| Represents an item in the `ListBlock`.

```markdown
* First `ListItem` content for this unordered `ListBlock`
    Child element for the first `ListItem`.

    1. This `ListItem` is in the child `ListBlock` for the First `ListItem` content.
    1. This `ListItem` is also in the child `ListBlock` for the First `ListItem` content.

* Second `ListItem` content for this unordered `ListBlock`
```

-}
type alias ListItem =
    { content : List InlineElement
    , children : List BlockElement
    }



-- # Renderer


{-| Render `Section` as markdown text.
-}
render : Section -> String
render =
    sectionToString { headerLevel = 1 }


sectionToString : SectionStringContext -> Section -> String
sectionToString context (Section sec) =
    let
        childContext =
            { context | headerLevel = context.headerLevel + 1 }
    in
    List.concat
        [ [ String.join " "
                [ String.repeat context.headerLevel "#"
                , String.trim sec.title
                ]
          ]
        , List.map
            (blockToString
                { indentLevel = 0
                }
            )
            sec.body
        , List.map
            (sectionToString childContext)
            sec.children
        ]
        |> String.join "\n\n"


type alias SectionStringContext =
    { headerLevel : Int
    }


blockToString : BlockStringContext -> BlockElement -> String
blockToString context block =
    let
        setIndent : String -> String
        setIndent str =
            String.concat
                [ String.repeat context.indentLevel "    "
                , str
                ]
    in
    case block of
        ParagraphBlock ps ->
            List.foldl
                (\inline lcontext ->
                    if inline == LineBreak then
                        { fixed =
                            (lcontext.ongoing
                                |> List.reverse
                                |> String.concat
                                |> (\str -> str ++ "  ")
                            )
                                :: lcontext.fixed
                        , ongoing = []
                        }

                    else
                        { fixed = lcontext.fixed
                        , ongoing = inlineToString inline :: lcontext.ongoing
                        }
                )
                { fixed = []
                , ongoing = []
                }
                ps
                |> (\lcontext ->
                        if lcontext.ongoing == [] then
                            lcontext.fixed

                        else
                            (List.reverse lcontext.ongoing
                                |> String.concat
                            )
                                :: lcontext.fixed
                   )
                |> List.reverse
                |> List.map setIndent
                |> String.join "\n"

        ListBlock param ->
            List.map
                (listItemToString param.ordered context)
                param.items
                |> String.join "\n"

        CodeBlock param ->
            let
                ( header, codeLines ) =
                    case String.lines param of
                        [] ->
                            ( "", [] )

                        h :: bs ->
                            let
                                rawBody =
                                    trimIndent bs

                                hasEmptyLastLine =
                                    List.drop
                                        (List.length rawBody - 1)
                                        rawBody
                                        == [ "" ]
                            in
                            ( String.trim h
                            , rawBody
                                |> (if hasEmptyLastLine then
                                        List.take (List.length rawBody - 1)

                                    else
                                        identity
                                   )
                            )

                backslashes : Int
                backslashes =
                    List.map headTicks codeLines
                        |> List.maximum
                        |> Maybe.map
                            (\n -> n + 1)
                        |> Maybe.withDefault 3
                        |> max 3
            in
            String.join "\n"
                [ String.concat
                    [ String.repeat backslashes "`"
                    , String.trim header
                    ]
                    |> setIndent
                , List.map setIndent codeLines
                    |> String.join "\n"
                , String.repeat backslashes "`"
                    |> setIndent
                ]

        QuoteBlock blocks ->
            List.map
                (blockToString
                    { indentLevel = 0
                    }
                )
                blocks
                |> List.intersperse ""
                |> List.concatMap String.lines
                |> List.map
                    (\line ->
                        if line == "" then
                            ">"

                        else
                            "> " ++ line
                    )
                |> List.map setIndent
                |> String.join "\n"


normalizeText : String -> String
normalizeText =
    normalizedWords
        >> String.join " "


normalizedWords : String -> List String
normalizedWords =
    -- `String.words` does not recognize 0x0085 as whitespace.
    String.map
        (\c ->
            if Char.toCode c == 0x85 then
                ' '

            else
                c
        )
        -- Escape special characters
        >> String.foldr
            (\c acc ->
                if List.member c specialCharacters then
                    String.cons '\\' (String.cons c acc)

                else
                    String.cons c acc
            )
            ""
        >> String.words


specialCharacters : List Char
specialCharacters =
    [ '\\'
    , '`'
    , '*'
    , '_'
    , '{'
    , '}'
    , '['
    , ']'
    , '('
    , ')'
    , '#'
    , '+'
    , '-'
    , '!'
    , '>'
    , '~'
    ]


normalizePlainText : String -> String
normalizePlainText rawStr =
    let
        hasHeadSpace =
            String.left 1 rawStr
                |> String.any isWhiteSpace

        hasLastSpace =
            String.right 1 rawStr
                -- `|> String.any isWhiteSpace` may cause infinite loop
                |> String.foldl
                    (\c acc ->
                        acc || isWhiteSpace c
                    )
                    False

        words =
            case normalizedWords rawStr of
                [] ->
                    []

                w :: ws ->
                    escapeDummyOrderedList w :: ws
    in
    List.concat
        [ if hasHeadSpace then
            [ "" ]

          else
            []
        , words
        , if hasLastSpace then
            [ "" ]

          else
            []
        ]
        |> String.join " "


escapeDummyOrderedList : String -> String
escapeDummyOrderedList str =
    if String.startsWith "." str then
        str

    else
        String.foldl
            (\c context ->
                case context.digits of
                    Nothing ->
                        { acc = String.cons c context.acc
                        , digits = Nothing
                        }

                    Just digits ->
                        if Char.isDigit c then
                            { acc = context.acc
                            , digits =
                                Just <| String.cons c digits
                            }

                        else if c == '.' then
                            { acc = ".\\" ++ digits
                            , digits = Nothing
                            }

                        else
                            { acc = String.cons c digits
                            , digits = Nothing
                            }
            )
            { acc = "" -- reversed
            , digits = Just "" -- reversed
            }
            str
            |> .acc
            |> String.reverse


{-| Characters considered "whitespace" by `String.words`
-}
isWhiteSpace : Char -> Bool
isWhiteSpace c =
    List.member
        (Char.toCode c)
        [ 0x09
        , 0x0A
        , 0x0B
        , 0x0C
        , 0x0D
        , 0x20
        , 0x85
        , 0xA0
        , 0x1680
        , 0x2000
        , 0x2001
        , 0x2002
        , 0x2003
        , 0x2004
        , 0x2005
        , 0x2006
        , 0x2007
        , 0x2008
        , 0x2009
        , 0x200A
        , 0x2028
        , 0x2029
        , 0x202F
        , 0x205F
        , 0x3000
        ]


type alias BlockStringContext =
    { indentLevel : Int
    }


listItemToString : Bool -> BlockStringContext -> ListItem -> String
listItemToString ordered context item =
    let
        symbol =
            if ordered then
                "1."

            else
                "*"

        setIndent : String -> String
        setIndent str =
            String.concat
                [ String.repeat context.indentLevel "    "
                , str
                ]

        hasBlankLine =
            case item.children of
                [] ->
                    False

                (ListBlock _) :: _ ->
                    False

                _ ->
                    True
    in
    List.concat
        [ [ String.join " "
                [ symbol
                , List.map inlineToString item.content
                    |> String.concat
                ]
                |> setIndent
          ]
        , if hasBlankLine then
            [ "" ]

          else
            []
        , List.map
            (blockToString
                { context | indentLevel = context.indentLevel + 1 }
            )
            item.children
            |> List.intersperse ""
        ]
        |> String.join "\n"


inlineToString : InlineElement -> String
inlineToString inline =
    case inline of
        PlainText text ->
            normalizePlainText text

        Link param ->
            String.concat
                [ "["
                , normalizeText param.text
                , "]("
                , List.filterMap identity
                    [ Just param.href
                    , param.title
                        |> Maybe.map
                            (\title ->
                                String.concat
                                    [ "\""
                                    , normalizeText title
                                        |> escapeDoubleQuote
                                    , "\""
                                    ]
                            )
                    ]
                    |> String.join " "
                , ")"
                ]

        Image param ->
            String.concat
                [ "!["
                , normalizeText param.alt
                , "]("
                , List.filterMap identity
                    [ Just param.src
                    , param.title
                        |> Maybe.map
                            (\title ->
                                String.concat
                                    [ "\""
                                    , normalizeText title
                                        |> escapeDoubleQuote
                                    , "\""
                                    ]
                            )
                    ]
                    |> String.join " "
                , ")"
                ]

        InlineCode code ->
            let
                backslashes =
                    inlineTicks code + 1
            in
            String.concat
                [ String.repeat backslashes "`"
                , code
                , String.repeat backslashes "`"
                ]

        Emphasis str ->
            String.concat
                [ "*"
                , normalizeText str
                , "*"
                ]

        StrongEmphasis str ->
            String.concat
                [ "**"
                , normalizeText str
                , "**"
                ]

        Strikethrough str ->
            String.concat
                [ "~~"
                , normalizeText str
                , "~~"
                ]

        LineBreak ->
            ""


escapeDoubleQuote : String -> String
escapeDoubleQuote =
    String.foldr
        (\c acc ->
            if c == '"' then
                String.cons '\\' (String.cons c acc)

            else
                String.cons c acc
        )
        ""



-- # Preview


{-| Preview markdown content as an HTML page.
-}
preview : Section -> Html msg
preview =
    previewSection
        { headerLevel = 1
        }


previewSection : SectionPreviewContext -> Section -> Html msg
previewSection context (Section sec) =
    List.concat
        [ [ previewHeader context.headerLevel sec.title
          ]
        , List.map previewBlock sec.body
        , List.map
            (previewSection
                { context
                    | headerLevel =
                        context.headerLevel + 1
                }
            )
            sec.children
        ]
        |> Html.div []


type alias SectionPreviewContext =
    { headerLevel : Int
    }


previewHeader : Int -> String -> Html msg
previewHeader n str =
    let
        attr =
            [ Attributes.id <| Url.percentEncode str
            ]
    in
    case n of
        1 ->
            Html.h1 attr [ Html.text str ]

        2 ->
            Html.h2 attr [ Html.text str ]

        3 ->
            Html.h3 attr [ Html.text str ]

        4 ->
            Html.h4 attr [ Html.text str ]

        5 ->
            Html.h5 attr [ Html.text str ]

        6 ->
            Html.h6 attr [ Html.text str ]

        _ ->
            Html.div attr [ Html.text str ]


previewBlock : BlockElement -> Html msg
previewBlock block =
    case block of
        ParagraphBlock ps ->
            List.map previewInline ps
                |> Html.p []

        ListBlock param ->
            let
                container =
                    if param.ordered then
                        Html.ol

                    else
                        Html.ul
            in
            List.map previewListItem param.items
                |> container []

        CodeBlock param ->
            let
                ( header, codeLines ) =
                    case String.lines param of
                        [] ->
                            ( "", [] )

                        h :: bs ->
                            let
                                rawBody =
                                    trimIndent bs

                                hasEmptyLastLine =
                                    List.drop
                                        (List.length rawBody - 1)
                                        rawBody
                                        == [ "" ]
                            in
                            ( String.trim h
                            , rawBody
                                |> (if hasEmptyLastLine then
                                        List.take (List.length rawBody - 1)

                                    else
                                        identity
                                   )
                            )
            in
            [ Html.text <|
                String.join "\n" codeLines
            ]
                |> Html.code [ Attributes.attribute "data-code-header" header ]
                |> List.singleton
                |> Html.pre []

        QuoteBlock blocks ->
            List.map previewBlock blocks
                |> Html.blockquote []


previewListItem : ListItem -> Html msg
previewListItem item =
    List.concat
        [ [ List.map previewInline item.content
                |> Html.p []
          ]
        , List.map previewBlock item.children
        ]
        |> Html.li []


previewInline : InlineElement -> Html msg
previewInline inline =
    case inline of
        PlainText text ->
            Html.text text

        Link param ->
            Html.a
                (List.filterMap identity
                    [ Just <| Attributes.href param.href
                    , Maybe.map Attributes.title param.title
                    ]
                )
                [ Html.text param.text
                ]

        Image param ->
            Html.img
                (List.filterMap identity
                    [ Just <| Attributes.src param.src
                    , Maybe.map Attributes.title param.title
                    , Just <| Attributes.alt param.alt
                    ]
                )
                []

        InlineCode code ->
            Html.code [] [ Html.text code ]

        Emphasis text ->
            Html.em [] [ Html.text text ]

        StrongEmphasis text ->
            Html.strong [] [ Html.text text ]

        Strikethrough text ->
            Html.del [] [ Html.text text ]

        LineBreak ->
            Html.br [] []
