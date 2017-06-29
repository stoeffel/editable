module App exposing (..)

import Editable exposing (Editable(..), Locked(..), Unlocked(..))
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model locked unlocked =
    { message : Editable locked unlocked String
    , logo : String
    }


init : String -> ( Model Locked never, Cmd Msg )
init path =
    ( { message = Editable.readOnly "Your Elm App is working!"
      , logo = path
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UnlockMessage


update : Msg -> Model locked unlocked -> ( Model locked unlocked, Cmd Msg )
update msg model =
    case msg of
        UnlockMessage ->
            ( { model
                | message =
                    Editable.edit model.message
              }
            , Cmd.none
            )



---- VIEW ----


view : Model locked unlocked -> Html Msg
view model =
    div []
        [ img [ src model.logo ] []
        , Editable.fold
            viewMessage
            viewEditMessage
            model.message
        , Html.button [ onClick UnlockMessage ] [ Html.text "unlock" ]
        ]


viewEditMessage : Editable never Unlocked String -> Html Msg
viewEditMessage message =
    Editable.modifiedValue message
        |> Html.text
        |> List.singleton
        |> Html.input []


viewMessage : Editable Locked never String -> Html Msg
viewMessage message =
    Editable.readValue message
        |> Html.text
        |> List.singleton
        |> Html.p []



---- PROGRAM ----


main : Program String (Model Locked never) Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
