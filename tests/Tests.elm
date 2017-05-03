module Tests exposing (..)

import Test exposing (..)
import Expect
import Editable


all : Test
all =
    describe "Editable"
        [ describe "#init"
            [ test "Creates a unsaved Editable" <|
                \() ->
                    Editable.init "Hello"
                        |> Expect.all
                            [ Editable.isSaved >> Expect.equal False
                            , Editable.hasChanged >> Expect.equal False
                            , Editable.edit "foo" >> Editable.hasChanged >> Expect.equal True
                            ]
            ]
        , describe "#save"
            [ test "saves an unsaved Editable" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.save
                        |> Expect.all
                            [ Editable.isSaved >> Expect.equal True
                            , Editable.hasChanged >> Expect.equal False
                            ]
            ]
        , describe "#cancel"
            [ test "reverts an unsaved Editable back to it's original value" <|
                \() ->
                    Editable.init "Hello"
                        |> Editable.edit "foo"
                        |> Editable.cancel
                        |> Expect.all
                            [ Editable.isSaved >> Expect.equal True
                            , Editable.hasChanged >> Expect.equal False
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
                            , Editable.hasChanged
                                >> Expect.equal True
                            ]
            ]
        ]
