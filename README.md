Editable
========

> Editable represents a value that can be modified. It offers a function to either save or cancel a changed value. This means the value has two different states `Saved` and `Unsaved`.
`Saved a` holds the saved value and `Unsaved a a`  holds both the saved and the newly modified value.


## Installation

`elm-package install stoeffel/editable`


## Usage

### Creating Editables

```elm
Editable.init "Hello"        -- Unsaved "Hello" "Hello"
    |> Editable.edit "World" -- Unsaved "Hello" "World"
    |> Editable.save         -- Saved "World"

Editable.init "Hello"        -- Unsaved "Hello" "Hello"
    |> Editable.edit "World" -- Unsaved "Hello" "World"
    |> Editable.cancel       -- Saved "Hello"
```


### Usage in View

```elm
view : Editable String -> Html msg
view editable =
    case editable of
        Unsaved saved modified ->
            input [ defaultValue modified ] []
        Saved saved ->
            text saved
```
