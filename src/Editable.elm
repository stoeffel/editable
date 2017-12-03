module Editable
    exposing
        ( Editable
        , cancel
        , edit
        , editable
        , isDirty
        , isEditable
        , isReadOnly
        , map
        , readonly
        , save
        , value
        )

{-| Editable represents a value that can be read-only or editable.
`ReadOnly a` holds the locked value and `Editable a a` holds both the old and the newly modified value.

@docs Editable, editable, readonly, cancel, edit, isDirty, isEditable, isReadOnly, map, save, value

-}

import Lockable exposing (Lockable)
import Saved exposing (Eq, Saved)


{-| An `Editable` value is either `ReadOnly` or `Editable`.

    view : Editable String -> Html msg
    view editable =
        case editable of
            Editable saved modified ->
                input [ defaultValue modified ] []

            ReadOnly saved ->
                text saved

-}
type alias Editable a =
    Lockable (Saved a)


{-| Makes a `ReadOnly` value `Editable`.

    Editable.ReadOnly "old"
        |> Editable.edit
        |> Editable.map (always "new") --> Editable "old" "new"

-}
edit : Editable a -> Editable a
edit =
    Lockable.unlock


{-| Apply a function to an `Editable`. This is the function you will call in
order to update the value of an `Editable.Editable`.

    Editable.ReadOnly "old"
        |> Editable.map String.toUpper --> ReadOnly "old"

    Editable.Editable "old" "old"
        |> Editable.map String.toUpper --> Editable "old" "OLD"

    Editable.Editable "old" "new"
        |> Editable.map (\val -> val ++ "er")  --> Editable "old" "newer"

    Editable.Editable "old" "old"
        |> Editable.map (always "new") --> Editable "old" "new"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    Lockable.change (Saved.change f) x


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
    Lockable.map Saved.save x
        |> Lockable.lock


{-| Cancels a modified value. This puts the old value into the context of `ReadOnly`.

    Editable.Editable "old" "new"
        |> Editable.cancel       --> ReadOnly "old"

-}
cancel : Editable a -> Editable a
cancel x =
    Lockable.map Saved.discard x
        |> Lockable.lock


{-| Returns the current value of an Editable.

    Editable.ReadOnly "old"
        |> Editable.value  --> "old"

    Editable.Editable "old" "new"
        |> Editable.value  --> "new"

-}
value : Editable a -> a
value =
    Lockable.value >> Saved.value


{-| Indicates if an `Editable` is in `Editable` state.

    Editable.Editable "old" "old"
        |> Editable.isEditable  --> True

    Editable.ReadOnly "old"
        |> Editable.isEditable  --> False

-}
isEditable : Editable a -> Bool
isEditable =
    not << Lockable.locked


{-| Indicates if an `Editable` is in `ReadOnly` state.

    Editable.Editable "old" "old"
        |> Editable.isReadOnly  --> False

    Editable.ReadOnly "old"
        |> Editable.isReadOnly  --> True

-}
isReadOnly : Editable a -> Bool
isReadOnly =
    Lockable.locked


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
isDirty =
    not << Saved.saved << Lockable.value


{-| Create a new `Editable` value that starts out in a `ReadOnly` state.
-}
readonly : Eq a -> a -> Editable a
readonly eq x =
    Saved.new eq x
        |> Lockable.new


{-| Create a new `Editable` value that starts out in an `Editable` state.
-}
editable : Eq a -> a -> Editable a
editable eq x =
    readonly eq x
        |> edit
