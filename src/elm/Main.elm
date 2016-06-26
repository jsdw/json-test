import Html.App

import Html exposing (Html)
import JsonTest exposing (init, update, view)
import Task exposing (Task)

main =
    Html.App.program
      { init = init
      , update = update
      , view = view
      , subscriptions = \_ -> Sub.none
      }
