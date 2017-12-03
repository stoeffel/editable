module Tests exposing (..)

import Editable exposing (Editable, editable, readonly)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Editable"
        [ describe "#edit"
            [ fuzz string "changes a read only value into an editable value" <|
                \value ->
                    readonly (==) value
                        |> Editable.edit
                        |> Editable.isEditable
                        |> Expect.equal True
            , fuzz string "nothing changes if it's already a editable value" <|
                \value ->
                    editable (==) value
                        |> Editable.edit
                        |> Editable.isEditable
                        |> Expect.equal True
            ]
        , describe "#map"
            [ fuzz string "map doesn't apply the function to a read only value" <|
                \value ->
                    readonly (==) value
                        |> Editable.map String.toUpper
                        |> Editable.value
                        |> Expect.equal value
            , fuzz string "map applies the function to a editable value" <|
                \value ->
                    editable (==) value
                        |> Editable.map String.toUpper
                        |> Editable.value
                        |> Expect.equal (String.toUpper value)
            ]
        , describe "functor"
            [ fuzz string "identity" <|
                \x ->
                    readonly (==) x
                        |> Editable.map identity
                        |> Editable.value
                        |> Expect.equal x
            , fuzz string "identity (editable)" <|
                \x ->
                    editable (==) x
                        |> Editable.map identity
                        |> Editable.value
                        |> Expect.equal x
            , fuzz string "composition" <|
                \x ->
                    readonly (==) x
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Editable.value
                        |> Expect.equal
                            (readonly (==) x
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                                |> Editable.value
                            )
            , fuzz string "composition (editable)" <|
                \x ->
                    editable (==) x
                        |> Editable.map ((++) "!" >> (++) "?")
                        |> Editable.value
                        |> Expect.equal
                            (editable (==) x
                                |> Editable.map ((++) "!")
                                |> Editable.map ((++) "?")
                                |> Editable.value
                            )
            ]
        , describe "#save"
            [ fuzz2 string string "save makes a editable read only with the modified value." <|
                \a b ->
                    readonly (==) a
                        |> Editable.edit
                        |> Editable.map (always b)
                        |> Editable.save
                        |> Expect.all
                            [ Editable.value >> Expect.equal b
                            , Editable.isEditable >> Expect.equal False
                            ]
            ]
        , describe "#cancel"
            [ fuzz2 string string "cancels a change to a editable and  makes it a read only with the old value." <|
                \a b ->
                    readonly (==) a
                        |> Editable.edit
                        |> Editable.map (always b)
                        |> Editable.cancel
                        |> Expect.all
                            [ Editable.value >> Expect.equal a
                            , Editable.isEditable >> Expect.equal False
                            ]
            ]
        , describe "#value"
            [ fuzz2 string string "returns the modified value of a Editable." <|
                \a b ->
                    editable (==) a
                        |> Editable.map (always b)
                        |> Editable.value
                        |> Expect.equal b
            , fuzz string "returns the value of a read only." <|
                \a ->
                    readonly (==) a
                        |> Editable.value
                        |> Expect.equal a
            ]
        , describe "#isDirty"
            [ fuzz2 string string "indicates if a modified value is different from the saved one." <|
                \a b ->
                    editable (==) a
                        |> Editable.map (always b)
                        |> Editable.isDirty
                        |> Expect.equal (a /= b)
            , fuzz2 string string "return False if `editable` is read only." <|
                \a b ->
                    editable (==) a
                        |> Editable.map (always b)
                        |> Editable.edit
                        |> Editable.save
                        |> Editable.isDirty
                        |> Expect.equal False
            ]
        , describe "#isEditable"
            [ fuzz2 string string "returns True if an `Editable` is in editable state." <|
                \a b ->
                    editable (==) a
                        |> Editable.map (always b)
                        |> Editable.isEditable
                        |> Expect.equal True
            , fuzz string "returns False if an `Editable` is in read only state." <|
                \x ->
                    readonly (==) x
                        |> Editable.isEditable
                        |> Expect.equal False
            ]
        , describe "#isReadOnly"
            [ fuzz2 string string "returns False if an `Editable` is in editable state." <|
                \a b ->
                    editable (==) a
                        |> Editable.map (always b)
                        |> Editable.isReadOnly
                        |> Expect.equal False
            , fuzz string "returns True if an `Editable` is in read only state." <|
                \x ->
                    readonly (==) x
                        |> Editable.isReadOnly
                        |> Expect.equal True
            ]
        ]
