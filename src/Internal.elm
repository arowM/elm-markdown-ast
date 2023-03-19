module Internal exposing
    ( headTicks
    , inlineTicks
    , trimIndent
    )

{-| Count head ticks

    headTicks "\n ```foo"
    --> 3

    headTicks "foo```"
    --> 0

-}


headTicks : String -> Int
headTicks str =
    let
        folded =
            String.foldl
                (\c acc ->
                    Result.andThen
                        (\n ->
                            if c == '`' then
                                -- May have another backslashes
                                Ok (n + 1)

                            else
                                -- End of backslash sequence
                                Err n
                        )
                        acc
                )
                (Ok 0)
                (String.trim str)
    in
    case folded of
        Ok n ->
            n

        Err n ->
            n


{-|

    inlineTicks "foo`bar``baz"
    --> 2

    inlineTicks "foobarbaz"
    --> 0

-}
inlineTicks : String -> Int
inlineTicks str =
    let
        ( n1, n2 ) =
            String.foldl
                (\c ( best, curr ) ->
                    if c == '`' then
                        ( best, curr + 1 )

                    else
                        ( max best curr, 0 )
                )
                ( 0, 0 )
                str
    in
    max n1 n2


{-|

    trimIndent
        [ "    foo bar"
        , "    bar baz baz"
        , "        and bar"
        ]
    --> [ "foo bar"
    --> , "bar baz baz"
    --> , "    and bar"
    --> ]

-}
trimIndent : List String -> List String
trimIndent ls =
    case ls of
        [] ->
            []

        s :: _ ->
            let
                indent =
                    headSpaces s
            in
            List.map (String.dropLeft indent) ls


headSpaces : String -> Int
headSpaces str =
    let
        folded =
            String.foldl
                (\c acc ->
                    Result.andThen
                        (\n ->
                            if c == ' ' then
                                -- May have another spaces
                                Ok (n + 1)

                            else
                                -- End of space sequence
                                Err n
                        )
                        acc
                )
                (Ok 0)
                str
    in
    case folded of
        Ok n ->
            n

        Err n ->
            n
