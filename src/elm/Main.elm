import Signal exposing (Signal)
import Html exposing (Html)
import Effects exposing (Never)
import JsonTest exposing (init, update, view)
import StartApp
import Task


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main = app.html


port tasks : Signal (Task.Task Never ())
port tasks = app.tasks