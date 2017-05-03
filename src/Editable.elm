module Editable
    exposing
        ( Editable
        , init
        , isSaved
        , save
        , cancel
        , edit
        , value
        , hasChanged
        )


type Editable a
    = Unsaved a a
    | Saved a


init : a -> Editable a
init x =
    Unsaved x x


isSaved : Editable a -> Bool
isSaved editable =
    case editable of
        Unsaved _ _ ->
            False

        Saved _ ->
            True


save : Editable a -> Editable a
save x =
    case x of
        Unsaved old new ->
            Saved new

        Saved _ ->
            x


cancel : Editable a -> Editable a
cancel x =
    case x of
        Unsaved old _ ->
            Saved old

        Saved _ ->
            x


map : (a -> a) -> Editable a -> Editable a
map f x =
    case x of
        Unsaved old x ->
            Unsaved old (f x)

        Saved old ->
            Unsaved old (f old)


edit : a -> Editable a -> Editable a
edit new x =
    case x of
        Unsaved old _ ->
            Unsaved old new

        Saved old ->
            Unsaved old new


value : Editable a -> a
value x =
    case x of
        Unsaved old new ->
            new

        Saved old ->
            old


hasChanged : Editable a -> Bool
hasChanged x =
    case x of
        Unsaved old new ->
            old /= new

        Saved _ ->
            False
