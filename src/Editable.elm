module Editable
    exposing
        ( Editable(Editable, ReadOnly)
        , cancel
        , edit
        , isDirty
        , isDirtyWith
        , isEditable
        , isReadOnly
        , map
        , save
        , value
        )

{-| Editable represents a value that can be read-only or editable.
`ReadOnly a` holds the locked value and `Editable a a` holds both the old and the newly modified value.

@docs Editable, cancel, edit, isDirty, isDirtyWith, isEditable, isReadOnly, map, save, value

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

    Editable.ReadOnly "old"
        |> Editable.edit
        |> Editable.map (always "new") --> Editable "old" "new"

-}
edit : Editable a -> Editable a
edit x =
    case x of
        Editable _ _ ->
            x

        ReadOnly value ->
            Editable value value


{-| Apply a function to an `Editable`. This is the function you will call in
order to update the value of an `Editable.Editable`.

    Editable.ReadOnly "old"
        |> Editable.map String.toUpper --> ReadOnly "old"

    Editable.Editable "old" "old"
        |> Editable.map String.toUpper --> Editable "old" "OLD"

    Editable.Editable "old" "new"
        |> Editable.map (\val -> val ++ "er")  --> Editable "old" "newer"

    Editable.Editable "old" "new"
        |> Editable.map (always "new") --> Editable "old" "new"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    case x of
        Editable saved modified ->
            Editable saved (f modified)

        ReadOnly saved ->
            ReadOnly saved


{-| Save a modified value. This puts the modified value into the context of `ReadOnly`.

    Editable.Editable "old" "new"
        |> Editable.save          --> ReadOnly "new"

    Editable.ReadOnly "old"
        |> Editable.edit
        |> Editable.map (always "new")
        |> Editable.save          --> ReadOnly "new"

-}
save : Editable a -> Editable a
save x =
    case x of
        Editable _ modified ->
            ReadOnly modified

        ReadOnly _ ->
            x


{-| Cancels a modified value. This puts the old value into the context of `ReadOnly`.

    Editable.Editable "old" "new"
        |> Editable.cancel       --> ReadOnly "old"

-}
cancel : Editable a -> Editable a
cancel x =
    case x of
        Editable value _ ->
            ReadOnly value

        ReadOnly _ ->
            x


{-| Returns the current value of an Editable.

    Editable.ReadOnly "old"
        |> Editable.value  --> "old"

    Editable.Editable "old" "new"
        |> Editable.value  --> "new"

-}
value : Editable a -> a
value x =
    case x of
        Editable _ value ->
            value

        ReadOnly value ->
            value


{-| Indicates if an `Editable` is in `Editable` state.

    Editable.Editable "old" "old"
        |> Editable.isEditable  --> True

    Editable.ReadOnly "old"
        |> Editable.isEditable  --> False

-}
isEditable : Editable a -> Bool
isEditable x =
    case x of
        Editable _ _ ->
            True

        _ ->
            False


{-| Indicates if an `Editable` is in `ReadOnly` state.

    Editable.Editable "old" "old"
        |> Editable.isReadOnly  --> False

    Editable.ReadOnly "old"
        |> Editable.isReadOnly  --> True

-}
isReadOnly : Editable a -> Bool
isReadOnly x =
    case x of
        ReadOnly _ ->
            True

        _ ->
            False


{-| Indicates if a modified value has changed from the saved one, by checking equality of both values.

If the `Editable` is `ReadOnly` then we return False.

    Editable.Editable "old" "old"
        |> Editable.isDirty  --> False

    Editable.Editable "old" "new"
        |> Editable.isDirty  --> True

    Editable.ReadOnly "old"
        |> Editable.isDirty  --> False

-}
isDirty : Editable a -> Bool
isDirty x =
    isDirtyWith (/=) x


{-| Indicates if a modified value has changed from the saved one, by a provided function.

If the `Editable` is `ReadOnly` then we return False.

    Editable.Editable "old" "old"
        |> Editable.isDirtyWith (/=)  --> False

    Editable.Editable "old" "new"
        |> Editable.isDirtyWith (/=)  --> True

    Editable.ReadOnly "old"
        |> Editable.isDirtyWith (/=)  --> False

-}
isDirtyWith : (a -> a -> Bool) -> Editable a -> Bool
isDirtyWith f x =
    case x of
        ReadOnly _ ->
            False

        Editable saved modified ->
            f saved modified
