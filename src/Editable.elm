module Editable
    exposing
        ( Editable(..)
        , Editing
        , Reading
        , cancel
        , edit
        , map
        , readOnly
        , readValue
        , save
        , update
        , value
        )

{-| Editable represents a value that can be read-only or editable.
`ReadOnly a` holds the locked value and `Editable a a` holds both the old and the newly modified value.

@docs Editable, Editing, Reading, cancel, edit, map, readOnly, save, update, value, readValue

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
type Editable a editing reading
    = Editable a a editing
    | ReadOnly a reading


{-| A value that is currently edited.
-}
type alias Editing a =
    Editable a IsEditable Never


{-| A value that is currently not edited.
-}
type alias Reading a =
    Editable a Never NotEditable


type NotEditable
    = NotEditable


type IsEditable
    = IsEditable


{-| Makes a `Reading` value `Editing`.

    Editable.readOnly "old"
        |> Editable.edit         --> Editable "old" "old"
        |> Editable.update "new" --> Editable "old" "new"

-}
edit : Reading a -> Editing a
edit x =
    let
        xValue =
            value x
    in
    Editable xValue xValue IsEditable


{-| Apply a function to an `Editing`.

    Editable.Editable "old" "old"
        |> Editable.map String.toUpper --> Editable "old" "OLD" IsEditable

-}
map : (a -> a) -> Editing a -> Editing a
map =
    map_


map_ : (a -> a) -> Editable a editing reading -> Editable a editing reading
map_ f x =
    case x of
        Editable saved modified a ->
            Editable saved (f modified) a

        ReadOnly saved a ->
            ReadOnly saved a


{-| Updates an `Editable` and doesn't change a `ReadOnly`.

    Editable.readOnly "old"
        |> Editable.edit
        |> Editable.update "new"  --> Editable "old" "new"

-}
update : a -> Editing a -> Editing a
update value =
    map (always value)


{-| Save a modified value. This puts the modified value into the context of `ReadOnly`.

    Editable.readOnly "old"
        |> Editable.edit          --> Editable "old" "old"
        |> Editable.update "new"  --> Editable "old" "new"
        |> Editable.save          --> ReadOnly "new"

-}
save : Editing a -> Reading a
save =
    readOnly << value


{-| Cancels a modified value. This puts the old value into the context of `ReadOnly`.

    Editable.readOnly "old"
        |> Editable.edit          --> Editable "old" "old"
        |> Editable.update "new"  --> Editable "old" "new"
        |> Editable.cancel        --> ReadOnly "old"

-}
cancel : Editing a -> Reading a
cancel =
    readOnly << readValue


{-| Returns the current value of an Editable.

    Editable.readOnly "old"
        |> Editable.value  --> "old"

    Editable.readOnly "old"
        |> Editable.edit          --> Editable "old" "old"
        |> Editable.update "new"  --> Editable "old" "new"
        |> Editable.value         --> "new"

-}
value : Editable a editing reading -> a
value x =
    case x of
        Editable _ value _ ->
            value

        ReadOnly value _ ->
            value


{-| Returns the current value of an Editable.

    Editable.readOnly "old"
        |> Editable.readValue  --> "old"

    Editable.readOnly "old"
        |> Editable.edit          --> Editable "old" "old"
        |> Editable.update "new"  --> Editable "old" "new"
        |> Editable.readValue     --> "old"

-}
readValue : Editable a editing reading -> a
readValue x =
    case x of
        Editable value _ _ ->
            value

        ReadOnly value _ ->
            value


{-| Creates a Editable as a ReadOnly.
-}
readOnly : a -> Reading a
readOnly x =
    ReadOnly x NotEditable
