module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect
import Editable


all : Test
all =
    describe "Editable"
        [ describe "#map"
            [ fuzz string "map applies the function to a Editable value" <|
                \value ->
                    Editable.readOnly value
                        |> Editable.edit
                        |> Editable.map String.toUpper
                        |> Editable.save
                        |> Expect.equal (Editable.readOnly <| String.toUpper value)
            ]
        , describe "functor"
            [ fuzz string "identity" <|
                \x ->
                    Editable.readOnly x
                        |> Editable.edit
                        |> Editable.map identity
                        |> Expect.equal (Editable.edit <| Editable.readOnly x)
            , fuzz string "composition" <|
                \x ->
                    Editable.readOnly x
                        |> Editable.edit
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Expect.equal
                            (Editable.readOnly x
                                |> Editable.edit
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                            )
            ]
        , describe "#update"
            [ fuzz2 string string "update sets the value if it's Editable" <|
                \a b ->
                    Editable.readOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Expect.all
                            [ Editable.value >> Expect.equal b
                            , Editable.readValue >> Expect.equal a
                            ]
            ]
        , describe "#save"
            [ fuzz2 string string "save makes a Editable ReadOnly with the modified value." <|
                \a b ->
                    Editable.readOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.save
                        |> Expect.equal (Editable.readOnly b)
            ]
        , describe "#cancel"
            [ fuzz2 string string "cancels a change to a Editable and  makes it a ReadOnly with the old value." <|
                \a b ->
                    Editable.readOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.cancel
                        |> Expect.equal (Editable.readOnly a)
            ]
        , describe "#value"
            [ fuzz2 string string "returns the modified value of a Editable." <|
                \a b ->
                    Editable.readOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.value
                        |> Expect.equal b
            , fuzz string "returns the value of a ReadOnly." <|
                \a ->
                    Editable.readOnly a
                        |> Editable.value
                        |> Expect.equal a
            ]
        , describe "#readValue"
            [ fuzz2 string string "returns the locked value of a Editable." <|
                \a b ->
                    Editable.readOnly a
                        |> Editable.edit
                        |> Editable.update b
                        |> Editable.readValue
                        |> Expect.equal a
            , fuzz string "returns the value of a ReadOnly." <|
                \a ->
                    Editable.readOnly a
                        |> Editable.readValue
                        |> Expect.equal a
            ]
        ]
