module Editable
    exposing
        ( Editable(Unsaved, Saved)
        , cancel
        , edit
        , init
        , isSaved
        , map
        , save
        , toMaybe
        , value
        )

{-| Editable represents a value that can be modified. It offers a function to either save or cancel a changed value. This means the value has two different states `Saved` and `Unsaved`.
`Saved a` holds the saved value and `Unsaved a a`  holds both the saved and the newly modified value.

@docs Editable, cancel, edit, init, isSaved, map, save, toMaybe, value
-}


{-| An `Editable` value is either `Saved` or `Unsaved`.

    view : Editable String -> Html msg
    view editable =
        case editable of
            Unsaved saved modified ->
                input [ defaultValue modified ] []
            Saved saved ->
                text saved

-}
type Editable a
    = Unsaved a a
    | Saved a


{-| Create a `Editable` value. It is created as `Unsaved`.
-}
init : a -> Editable a
init x =
    Unsaved x x


{-| Save a modified value. This puts the modified value into the context of `Saved`.

    Editable.init "Hello"        -- Unsaved "Hello" "Hello"
        |> Editable.edit "World" -- Unsaved "Hello" "World"
        |> Editable.save         -- Saved "World"

-}
save : Editable a -> Editable a
save x =
    case x of
        Unsaved saved modified ->
            Saved modified

        Saved _ ->
            x


{-| Cancels a modified value. This puts the old value into the context of `Saved`.

    Editable.init "Hello"        -- Unsaved "Hello" "Hello"
        |> Editable.edit "World" -- Unsaved "Hello" "World"
        |> Editable.cancel       -- Saved "Hello"

-}
cancel : Editable a -> Editable a
cancel x =
    case x of
        Unsaved saved _ ->
            Saved saved

        Saved _ ->
            x


{-| Edit an `Editable` value.

    Editable.init "Hello"        -- Unsaved "Hello" "Hello"
        |> Editable.edit "World" -- Unsaved "Hello" "World"

    Editable.init "Hello"        -- Unsaved "Hello" "Hello"
        |> Editable.save         -- Saved "Hello"
        |> Editable.edit "World" -- Unsaved "Hello" "World"

-}
edit : a -> Editable a -> Editable a
edit modified x =
    case x of
        Unsaved saved _ ->
            Unsaved saved modified

        Saved saved ->
            Unsaved saved modified


{-| Extract the value of an `Editable`.

    Editable.init "Hello"        -- Unsaved "Hello" "Hello"
        |> Editable.edit "World" -- Unsaved "Hello" "World"
        |> Editable.value        -- "World"

    Editable.init "Hello" -- Unsaved "Hello" "Hello"
        |> Editable.save  -- Saved "Hello"
        |> Editable.value -- "Hello"

-}
value : Editable a -> a
value x =
    case x of
        Unsaved saved modified ->
            modified

        Saved saved ->
            saved


{-| Apply a function to an `Editable`.

    Editable.init "Hello"              -- Unsaved "Hello" "Hello"
        |> Editable.map String.toUpper -- Unsaved "Hello" "HELLO"

    Editable.init "Hello"              -- Unsaved "Hello" "Hello"
        |> Editable.edit "World"       -- Unsaved "Hello" "World"
        |> Editable.save               -- Saved "World"
        |> Editable.map String.toUpper -- Saved "World"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    case x of
        Unsaved saved modified ->
            Unsaved saved (f modified)

        Saved saved ->
            Saved saved


{-| Convert to a `Maybe`. It's `Nothing` if the value is `Unsaved`.

    Editable.init "Hello"   -- Unsaved "Hello" "Hello"
        |> Editable.toMaybe -- Nothing

    Editable.init "Hello"   -- Unsaved "Hello" "Hello"
        |> Editable.save    -- Saved "Hello"
        |> Editable.toMaybe -- Just "Hello"

-}
toMaybe : Editable a -> Maybe a
toMaybe x =
    case x of
        Unsaved _ _ ->
            Nothing

        Saved saved ->
            Just saved


{-| Check if a value is saved.

    Editable.init "Hello"   -- Unsaved "Hello" "Hello"
        |> Editable.isSaved -- False

    Editable.init "Hello"   -- Unsaved "Hello" "Hello"
        |> Editable.save    -- Saved "Hello"
        |> Editable.isSaved -- True

-}
isSaved : Editable a -> Bool
isSaved editable =
    case editable of
        Unsaved _ _ ->
            False

        Saved _ ->
            True
