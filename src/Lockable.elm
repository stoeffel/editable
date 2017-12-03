module Lockable exposing (Lockable, change, lock, locked, map, unlock, value)

{-| A value that can be locked for writing.

While the value is locked, 'change' operations on it will have no effect.

-}


type Lockable a
    = Locked a
    | Unlocked a


value : Lockable a -> a
value lockable =
    case lockable of
        Locked x ->
            x

        Unlocked x ->
            x


lock : Lockable a -> Lockable a
lock =
    value >> Locked


unlock : Lockable a -> Lockable a
unlock =
    value >> Unlocked


locked : Lockable a -> Bool
locked lockable =
    case lockable of
        Locked _ ->
            True

        Unlocked _ ->
            False


change : (a -> a) -> Lockable a -> Lockable a
change fn lockable =
    case lockable of
        Locked _ ->
            lockable

        Unlocked x ->
            Unlocked (fn x)


map : (a -> b) -> Lockable a -> Lockable b
map fn lockable =
    case lockable of
        Locked x ->
            Locked (fn x)

        Unlocked x ->
            Unlocked (fn x)
