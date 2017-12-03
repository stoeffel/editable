module Saved
    exposing
        ( Eq
        , Saved
        , change
        , discard
        , map
        , new
        , save
        , saved
        , setSaved
        , value
        )

{-| A type to keep track of whether a value has been saved.

The value wrapped in `Save` can be in one of three states.

  - Changed: This means changes have been made to a saved value.
  - Saved: This means a value has been saved, and not changed since.

The type takes an equality function to distinguish between the Changed and Saved
states.

@docs Saved
@docs Eq
@docs change
@docs discard
@docs map
@docs new
@docs save
@docs saved
@docs setSaved
@docs value

-}


{-| A type representing the saved state of a value.
-}
type Saved a
    = Saved (Eq a) a
    | Changed (Eq a) a a


{-| An equality check for a type.

In a lot of cases you can simply pass in (==), but if the type in your `Saved`
is / contains types that implement their own equality checks, such as `Dict` or
`Set`, you will need to use those.

-}
type alias Eq a =
    a -> a -> Bool


{-| Wrap a value in a `Saved`.

The value passed in is considered the first saved state.

-}
new : Eq a -> a -> Saved a
new =
    Saved


{-| Get back a value from a `Saved`.
-}
value : Saved a -> a
value saved =
    case saved of
        Saved _ latest ->
            latest

        Changed _ _ latest ->
            latest


initial : Saved a -> a
initial saved =
    case saved of
        Saved _ initial ->
            initial

        Changed _ initial _ ->
            initial


eq : Saved a -> Eq a
eq saved =
    case saved of
        Saved eq _ ->
            eq

        Changed eq _ _ ->
            eq


{-| Change a value.
-}
change : (a -> a) -> Saved a -> Saved a
change fn saved =
    let
        same : Eq a
        same =
            eq saved

        old : a
        old =
            initial saved

        new : a
        new =
            fn (value saved)
    in
    if same old new then
        Saved same old
    else
        Changed same old new


{-| Set the 'saved' value, keeping the current value unchanged.

You might use this is you know your backend has saved a value, but your user
might have made new changes in the meanwhile.

-}
setSaved : a -> Saved a -> Saved a
setSaved newInitial saved =
    new (eq saved) newInitial
        |> change (\_ -> value saved)


{-| Save the current value.
-}
save : Saved a -> Saved a
save saved =
    case saved of
        Saved eq new ->
            Saved eq new

        Changed eq _ new ->
            Saved eq new


{-| Discard any changes and reset to the saved value.
-}
discard : Saved a -> Saved a
discard saved =
    case saved of
        Saved eq saved ->
            Saved eq saved

        Changed eq saved _ ->
            Saved eq saved


{-| Check if any changes have been made to the value.
-}
saved : Saved a -> Bool
saved saved =
    case saved of
        Saved _ _ ->
            True

        Changed _ _ _ ->
            False


{-| Map over the values in a saved.

Beware that this changes both the initial and the changed value, and so should
not be used to make changes to the value (for that, use 'change').

As a rule of thumb, if your map function does not go from a type to another type
and so is `a -> a`, you probably don't want to use this.

Because this function returns a saved with a different type, this function
requires a new equality test for this type.

-}
map : Eq b -> (a -> b) -> Saved a -> Saved b
map newEq fn saved =
    case saved of
        Saved _ initial ->
            Saved newEq (fn initial)

        Changed _ initial latest ->
            Changed newEq (fn initial) (fn latest)
