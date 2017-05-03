module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect
import Editable


all : Test
all =
    describe "Editable"
        [ describe "#init"
            [ test "Creates a unsaved Editable" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.isSaved
                        >> Expect.equal False
            ]
        , describe "#save"
            [ test "saves an unsaved Editable" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.save
                        |> Editable.isSaved
                        >> Expect.equal True
            ]
        , describe "#cancel"
            [ test "reverts an unsaved Editable back to it's original value" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.edit "foo"
                        |> Editable.cancel
                        |> Expect.all
                            [ Editable.isSaved >> Expect.equal True
                            , Editable.value >> Expect.equal "Hello"
                            ]
            ]
        , describe "#edit"
            [ test "updates an unsaved value" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.edit "Foo"
                        |> Expect.all
                            [ Editable.isSaved
                                >> Expect.equal False
                            , Editable.cancel
                                >> Editable.value
                                >> Expect.equal "Hello"
                            , Editable.save
                                >> Editable.value
                                >> Expect.equal "Foo"
                            ]
            ]
        , describe "#toMaybe"
            [ test "returns Nothing if it's Unsaved" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.toMaybe
                        |> Expect.equal Nothing
            , test "returns Just if it's Saved" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.save
                        |> Editable.toMaybe
                        |> Expect.equal (Just "Hello")
            ]
        , describe "functor"
            [ fuzz string "identity" <|
                \x ->
                    Editable.init x
                        |> Editable.map identity
                        |> Expect.equal (Editable.init x)
            , fuzz string "identity (saved)" <|
                \x ->
                    Editable.init x
                        |> Editable.save
                        |> Editable.map identity
                        |> Expect.equal
                            (Editable.init x
                                |> Editable.save
                            )
            , fuzz string "composition" <|
                \x ->
                    Editable.init x
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Expect.equal
                            (Editable.init x
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                            )
            , fuzz string "composition (saved)" <|
                \x ->
                    Editable.init x
                        |> Editable.save
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Expect.equal
                            (Editable.init x
                                |> Editable.save
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                            )
            ]
        ]
