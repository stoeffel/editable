module Lockable exposing (Lockable, change, lock, locked, map, new, unlock, value)

{-| A value that can be locked for writing.

While the value is locked, 'change' operations on it will have no effect.

@docs Lockable
@docs change
@docs lock
@docs locked
@docs map
@docs unlock
@docs value
@docs new

-}


{-| A value that can be in a locked or unlocked state.
-}
type Lockable a
    = Locked a
    | Unlocked a


{-| Get the value in a `Lockable`.
-}
value : Lockable a -> a
value lockable =
    case lockable of
        Locked x ->
            x

        Unlocked x ->
            x


{-| Lock a value, ensuring `change` calls on it will have no effect.
-}
lock : Lockable a -> Lockable a
lock =
    value >> Locked


{-| Unlock a value, ensuring `change` calls will affect it again.
-}
unlock : Lockable a -> Lockable a
unlock =
    value >> Unlocked


{-| Check if a `Lockable` is currently locked.
-}
locked : Lockable a -> Bool
locked lockable =
    case lockable of
        Locked _ ->
            True

        Unlocked _ ->
            False


{-| Change the value in a `Lockable`. This will only have effect if the
`Lockable` is currently unlocked.
-}
change : (a -> a) -> Lockable a -> Lockable a
change fn lockable =
    case lockable of
        Locked _ ->
            lockable

        Unlocked x ->
            Unlocked (fn x)


{-| Map the value in a `Lockable`. Be careful, this will always work, regardless
of the current lock state!
-}
map : (a -> b) -> Lockable a -> Lockable b
map fn lockable =
    case lockable of
        Locked x ->
            Locked (fn x)

        Unlocked x ->
            Unlocked (fn x)


{-| Create a new `Lockable`. The new `Lockable` will start out in a locked
state.
-}
new : a -> Lockable a
new =
    Locked
