module Editable
    exposing
        ( Editable(Unsaved, Saved)
        , cancel
        , edit
        , init
        , isSaved
        , map
        , save
        , value
        )

{-| Editable represents a value that can be modified. It offers a function to either save or cancel a changed value. This means the value has two different states `Saved` and `Unsaved`.
`Saved a` holds the saved value and `Unsaved a a`  holds both the saved and the newly modified value.

@docs Editable, cancel, edit, init, isSaved, map, save, value
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

    Editable.init "hello"        -- Unsaved "hello" "hello"
        |> Editable.edit "world" -- Unsaved "hello" "world"
        |> Editable.save         -- Saved "world"

-}
save : Editable a -> Editable a
save x =
    case x of
        Unsaved saved modified ->
            Saved modified

        Saved _ ->
            x


{-| Cancels a modified value. This puts the old value into the context of `Saved`.

    Editable.init "hello"        -- Unsaved "hello" "hello"
        |> Editable.edit "world" -- Unsaved "hello" "world"
        |> Editable.cancel       -- Saved "hello"

-}
cancel : Editable a -> Editable a
cancel x =
    case x of
        Unsaved saved _ ->
            Saved saved

        Saved _ ->
            x


{-| Edit an `Editable` value.

    Editable.init "hello"        -- Unsaved "hello" "hello"
        |> Editable.edit "world" -- Unsaved "hello" "world"

    Editable.init "hello"        -- Unsaved "hello" "hello"
        |> Editable.save         -- Saved "hello"
        |> Editable.edit "world" -- Unsaved "hello" "world"

-}
edit : a -> Editable a -> Editable a
edit modified x =
    case x of
        Unsaved saved _ ->
            Unsaved saved modified

        Saved saved ->
            Unsaved saved modified


{-| Extract the value of an `Editable`.

    Editable.init "hello"        -- Unsaved "hello" "hello"
        |> Editable.edit "world" -- Unsaved "hello" "world"
        |> Editable.value        -- "world"

    Editable.init "hello" -- Unsaved "hello" "hello"
        |> Editable.save  -- Saved "hello"
        |> Editable.value -- "hello"

-}
value : Editable a -> a
value x =
    case x of
        Unsaved saved modified ->
            modified

        Saved saved ->
            saved


{-| Apply a function to an `Editable`.

    Editable.init "hello"              -- Unsaved "hello" "hello"
        |> Editable.map String.toUpper -- Unsaved "hello" "HELLO"

    Editable.init "hello"              -- Unsaved "hello" "hello"
        |> Editable.edit "world"       -- Unsaved "hello" "world"
        |> Editable.save               -- Saved "world"
        |> Editable.map String.toUpper -- Saved "world"

-}
map : (a -> a) -> Editable a -> Editable a
map f x =
    case x of
        Unsaved saved modified ->
            Unsaved saved (f modified)

        Saved saved ->
            Saved saved


{-| Check if a value is saved.

    Editable.init "hello"   -- Unsaved "hello" "hello"
        |> Editable.isSaved -- False

    Editable.init "hello"   -- Unsaved "hello" "hello"
        |> Editable.save    -- Saved "hello"
        |> Editable.isSaved -- True

-}
isSaved : Editable a -> Bool
isSaved editable =
    case editable of
        Unsaved _ _ ->
            False

        Saved _ ->
            True
