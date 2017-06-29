module Editable
    exposing
        ( Editable
        , Locked(..)
        , Unlocked(..)
        , edit
        , fold
        , modifiedValue
        , readOnly
        , readValue
        )

{-| Editable represents a value that can be read-only or editable.
`ReadOnly a` holds the locked value and `Editable a a` holds both the old and the newly modified value.

@docs Editable, readOnly, edit, modifiedValue

-}

import Equality exposing (Equal)


{-| An `Editable` value is either `ReadOnly` or `Editable`.
-}
type Editable locked unlocked ty
    = Editable (Equal unlocked Unlocked) ty ty
    | ReadOnly (Equal locked Locked) ty


{-| Marks an Editable as locked.
-}
type Locked
    = Locked


{-| Marks an Editable as unlocked.
-}
type Unlocked
    = Unlocked


{-| Create a readonly value.
-}
readOnly : ty -> Editable Locked never ty
readOnly value =
    ReadOnly Equality.refl value


{-| Make a readonly value editable.
-}
edit : Editable Locked never ty -> Editable never Unlocked ty
edit wrapped =
    let
        edit_ : ty -> Editable never Unlocked ty
        edit_ value =
            Editable Equality.refl value value
    in
    case wrapped of
        Editable _ _ _ ->
            Debug.crash "This can't ever happen!"

        ReadOnly _ value ->
            edit_ value


{-| Get the value of a editable value.

    Editable.readOnly "old"
        |> Editable.edit
        |> Editable.modifiedValue
    --> "old"

    Editable.readOnly "old"
        |> Editable.modifiedValue
    -- 💥 This won't compile!

-}
modifiedValue : Editable never Unlocked ty -> ty
modifiedValue wrapped =
    case wrapped of
        Editable _ _ value ->
            value

        ReadOnly _ _ ->
            Debug.crash "This can't ever happen!"


{-| Get the value of a readonly value.

    Editable.readOnly "old"
        |> Editable.readValue
    --> "old"

    Editable.readOnly "old"
        |> Editable.edit
        |> Editable.readValue
    -- 💥 This won't compile!

-}
readValue : Editable Locked never ty -> ty
readValue wrapped =
    case wrapped of
        Editable _ _ _ ->
            Debug.crash "This can't ever happen!"

        ReadOnly _ value ->
            value


{-| Fold an editable.
-}
fold :
    (Editable Locked never ty -> a)
    -> (Editable never Unlocked ty -> a)
    -> Editable locked unlocked ty
    -> a
fold fromLocked fromUnlocked wrapped =
    case wrapped of
        Editable proof old new ->
            fromUnlocked (Editable Equality.refl old new)

        ReadOnly proof value ->
            fromLocked (ReadOnly Equality.refl value)
