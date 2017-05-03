module Editable
    exposing
        ( Editable(Editable, ReadOnly)
        , cancel
        , edit
        , map
        , save
        , update
        )

{-| Editable represents a value that can be read-only or editable.
`ReadOnly a` holds the locked value and `Editable a a`  holds both the old and the newly modified value.

@docs Editable, cancel, edit, map, save, update
-}


{-| An `Editable` value is either `ReadOnly` or `Editable`.

    view : Editable String -> Html msg
    view editable =
        case editable of
            Editable saved modified ->
                input [ defaultValue modified ] []
            ReadOnly saved ->
                text saved

-}
type Editable a
    = Editable a a
    | ReadOnly a


{-| Makes a `ReadOnly` value `Editable`.

    Editable.ReadOnly "Hello"
        |> Editable.update "World" --> ReadOnly "Hello"
        |> Editable.edit           --> Editable "Hello" "Hello"
        |> Editable.update "World" --> Editable "Hello" "World"

-}
edit : Editable a -> Editable a
edit x =
    case x of
        Editable _ _ ->
            x

        ReadOnly value ->
            Editable value value


{-| Apply a function to an `Editable`.

    Editable.ReadOnly "Hello"
        |> Editable.map String.toUpper --> ReadOnly "Hello"

    Editable.Editable "Hello" "Hello"
        |> Editable.map String.toUpper --> Editable "Hello" "HELLO"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    case x of
        Editable saved modified ->
            Editable saved (f modified)

        ReadOnly saved ->
            ReadOnly saved


{-| Updates an `Editable` and doesn't change a `ReadOnly`.

    Editable.ReadOnly "Hello"
        |> Editable.update "World"    --> ReadOnly "Hello"

    Editable.Editable "Hello" "Hello"
        |> Editable.update "World"    --> Editable "Hello" "World"

-}
update : a -> Editable a -> Editable a
update value =
    map (always value)


{-| Save a modified value. This puts the modified value into the context of `ReadOnly`.

    Editable.Editable "Hello" "World"
        |> Editable.save --> ReadOnly "World"

    Editable.ReadOnly "Hello"
        |> Editable.edit           --> Editable "Hello" "Hello"
        |> Editable.update "World" --> Editable "Hello" "World"
        |> Editable.save           --> ReadOnly "World"

-}
save : Editable a -> Editable a
save x =
    case x of
        Editable _ modified ->
            ReadOnly modified

        ReadOnly _ ->
            x


{-| Cancels a modified value. This puts the old value into the context of `ReadOnly`.

    Editable.Editable "Hello" "World"
        |> Editable.cancel       --> ReadOnly "Hello"

-}
cancel : Editable a -> Editable a
cancel x =
    case x of
        Editable value _ ->
            ReadOnly value

        ReadOnly _ ->
            x
