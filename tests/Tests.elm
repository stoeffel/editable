module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect
import Editable exposing (Editable(..))


all : Test
all =
    describe "Editable"
        [ describe "#edit"
            [ fuzz string "changes a ReadOnly value into an Editable value" <|
                \value ->
                    ReadOnly value
                        |> Editable.edit
                        |> Expect.equal (Editable value value)
            , fuzz string "nothing changes if it's already a Editable value" <|
                \value ->
                    Editable value value
                        |> Editable.edit
                        |> Expect.equal (Editable value value)
            ]
        , describe "#map"
            [ fuzz string "map doesn't apply the function to a ReadOnly value" <|
                \value ->
                    ReadOnly value
                        |> Editable.map String.toUpper
                        |> Expect.equal (ReadOnly value)
            , fuzz string "map applies the function to a Editable value" <|
                \value ->
                    Editable value value
                        |> Editable.map String.toUpper
                        |> Expect.equal (Editable value (String.toUpper value))
            ]
        , describe "functor"
            [ fuzz string "identity" <|
                \x ->
                    ReadOnly x
                        |> Editable.map identity
                        |> Expect.equal (ReadOnly x)
            , fuzz string "identity (editable)" <|
                \x ->
                    Editable x x
                        |> Editable.map identity
                        |> Expect.equal (Editable x x)
            , fuzz string "composition" <|
                \x ->
                    ReadOnly x
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Expect.equal
                            (ReadOnly x
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                            )
            , fuzz string "composition (editable)" <|
                \x ->
                    Editable x x
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Expect.equal
                            (Editable x x
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                            )
            ]
        , describe "#update"
            [ fuzz2 string string "update doesn't set the value if it's ReadOnly" <|
                \a b ->
                    ReadOnly a
                        |> Editable.update b
                        |> Expect.equal (ReadOnly a)
            , fuzz2 string string "update sets the value if it's Editable" <|
                \a b ->
                    Editable a a
                        |> Editable.update b
                        |> Expect.equal (Editable a b)
            ]
        , describe "#save"
            [ fuzz2 string string "save makes a Editable ReadOnly with the modified value." <|
                \a b ->
                    ReadOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.save
                        |> Expect.equal (ReadOnly b)
            ]
        , describe "#cancel"
            [ fuzz2 string string "cancels a change to a Editable and  makes it a ReadOnly with the old value." <|
                \a b ->
                    ReadOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.cancel
                        |> Expect.equal (ReadOnly a)
            ]
        , describe "#value"
            [ fuzz2 string string "returns the modified value of a Editable." <|
                \a b ->
                    Editable a b
                        |> Editable.value
                        |> Expect.equal b
            , fuzz string "returns the value of a ReadOnly." <|
                \a ->
                    ReadOnly a
                        |> Editable.value
                        |> Expect.equal a
            ]
        , describe "#isDirty"
            [ fuzz2 string string "determines if a modified value is different from the saved one." <|
                \a b ->
                    Editable a b
                        |> Editable.isDirty
                        |> Expect.equal (a == b)
            , fuzz2 string string "return False if a `Editable` is `ReadOnly`." <|
                \a b ->
                    Editable a b
                        |> Editable.edit
                        |> Editable.save
                        |> Editable.isDirty
                        |> Expect.equal False
            ]
        ]
