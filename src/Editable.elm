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

    Editable.readonly (==) "old"
        |> Editable.edit
        |> Editable.map (always "new")
        |> Editable.value --> "new"

-}
edit : Editable a -> Editable a
edit =
    Lockable.unlock


{-| Apply a function to an `Editable`. This is the function you will call in
order to update the value of an `Editable.editable`.

    Editable.readonly (==) "old"
        |> Editable.map String.toUpper
        |> Editable.value --> "old"

    Editable.editable (==) "old"
        |> Editable.map String.toUpper
        |> Editable.value --> "OLD"

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.map (\val -> val ++ "er")
        |> Editable.value --> "newer"

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.value --> "new"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    Lockable.change (Saved.change f) x


{-| Save a modified value. This puts the modified value into the context of `ReadOnly`.

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.save
        |> Editable.value --> "new"

    Editable.readonly (==) "old"
        |> Editable.edit
        |> Editable.map (always "new")
        |> Editable.save
        |> Editable.value --> "new"

-}
save : Editable a -> Editable a
save x =
    Lockable.map Saved.save x
        |> Lockable.lock


{-| Cancels a modified value. This puts the old value into the context of `ReadOnly`.

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.cancel
        |> Editable.value --> "old"

-}
cancel : Editable a -> Editable a
cancel x =
    Lockable.map Saved.discard x
        |> Lockable.lock


{-| Returns the current value of an Editable.

    Editable.readonly (==) "old"
        |> Editable.value  --> "old"

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.value  --> "new"

-}
value : Editable a -> a
value =
    Lockable.value >> Saved.value


{-| Indicates if an `Editable` is in `Editable` state.

    Editable.editable (==) "old"
        |> Editable.isEditable  --> True

    Editable.readonly (==) "old"
        |> Editable.isEditable  --> False

-}
isEditable : Editable a -> Bool
isEditable =
    not << Lockable.locked


{-| Indicates if an `Editable` is in `ReadOnly` state.

    Editable.editable (==) "old"
        |> Editable.isReadOnly  --> False

    Editable.readonly (==) "old"
        |> Editable.isReadOnly  --> True

-}
isReadOnly : Editable a -> Bool
isReadOnly =
    Lockable.locked


{-| Indicates if a modified value has changed from the saved one, by checking equality of both values.

If the `Editable` is `ReadOnly` then we return False.

    Editable.editable (==) "old"
        |> Editable.isDirty  --> False

    Editable.editable (==) "old"
        |> Editable.map (always "new")
        |> Editable.isDirty  --> True

    Editable.readonly (==) "old"
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
