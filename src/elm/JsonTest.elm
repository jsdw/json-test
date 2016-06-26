module JsonTest exposing --where
  (init, update, view)

import Task exposing (onError, succeed, andThen)
import Http
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Debug

--
-- Model: We care about the path, request method, JSON to send,
-- and finally current response.
--

type alias Response = Maybe (Result Http.RawError Http.Response)

type alias Model =
    { path    : String
    , req     : String
    , verb    : String
    , res     : Response
    , loading : Bool
    }

init : (Model, Cmd Action)
init = (Model "" "" "GET" Nothing False, Cmd.none)

--
-- Update: react to events to update our model
--

type Action
    = Path String
    | ReqVerb String
    | ReqBody String
    | ResSuccess Http.Response
    | ResError Http.RawError
    | Submit

update : Action -> Model -> (Model, Cmd Action)
update action model =
  let
    noFx m = (m, Cmd.none)
  in case Debug.log "change" action of
    Path str ->
        noFx { model | path = str }
    ReqVerb m ->
        noFx { model | verb = m }
    ReqBody str ->
        noFx { model | req = str }
    ResSuccess res ->
        noFx { model | loading = False, res = Just (Ok res) }
    ResError err ->
        noFx { model | loading = False, res = Just (Err err) }
    Submit ->
        ({model | loading = True, res = Nothing }, makeRequestGiven model)

makeRequestGiven : Model -> Cmd Action
makeRequestGiven model =
  let
    headers =
        [ ("Content-Type", "application/json")
        ]
    request =
        { verb = model.verb
        , headers = headers
        , url = model.path
        , body = Http.string model.req
        }
  in
    Task.perform ResError ResSuccess <| Http.send Http.defaultSettings request

--
-- View: Display our app related lark:
--

view : Model -> Html Action
view model =
    div [ class "container" ]
        [ h1 [] [ text "JSON Tester" ]
        , table
            [ class "inputs" ]
            [ row "Path" "input-path" <|
                input
                    [ id "input-path", onInput Path, value model.path ]
                    []
            , row "Method" "input-method" <|
                select
                    [ id "input-method", onChange ReqVerb ]
                    [ option [ selected <| model.verb == "GET", value "GET" ] [ text "GET" ]
                    , option [ selected <| model.verb == "POST", value "POST" ] [ text "POST" ]
                    ]
            , row "JSON" "input-json" <|
                textarea [ onInput ReqBody, value model.req ] []
            , tr [ class "submit" ]
                [ td [] []
                , td [] [ button [ onClick Submit ] [ text "Submit" ] ]
                ]
            ]
        , div
            [ class "response" ]
            [ div
                [ class "progress", hidden (not model.loading) ]
                [ text "Fetching Response..." ]
            , div
                [ hidden (not <| isJust model.res) ]
                [ viewResponse model.res ]
            ]
        ]

isJust : Maybe a -> Bool
isJust m = case m of
    Nothing -> False
    _ -> True

row : String -> String -> Html a -> Html a
row name id input =
    tr [ class "row" ]
        [ td
            [ class "row-name"  ]
            [ label [ for id ] [ text name ] ]
        , td
            [ class "row-input" ]
            [ input ]
        ]

viewResponse : Response -> Html a
viewResponse res =
  let
    noResponse =
        text "No request made"
    responseError =
        text "There was a low level error processing your request."
    response details =
        table []
            [ row "Status" ""  <| text (toString details.status ++ ": " ++ details.statusText)
            , row "Headers" "" <| table [] <| List.map viewHeader <| Dict.toList details.headers
            , row "Body" ""    <| viewBody details.value
            ]
    viewHeader (name, value) =
        row name "" <| text value
    viewBody val = case val of
        Http.Text val -> text val
        _ -> text "Cannot view body: not a string"
  in case res of
    Nothing  -> noResponse
    Just res' ->
        case res' of
            Ok good -> response good
            Err Http.RawTimeout   -> text "There was a network timeout."
            Err Http.RawNetworkError -> text "There was a network error of some sort (bad URL?)"


onChange : (String -> action) -> Attribute action
onChange tagger = on "change" (Json.map tagger <| Json.at ["target", "value"] Json.string)





