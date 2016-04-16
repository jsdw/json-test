module JsonTest (init, update, view) where

import Effects exposing (Effects, Never)
import Task exposing (onError, succeed, andThen)
import Signal exposing (Address)
import Http
import Effects
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--
-- Model: We care about the path, request method, JSON to send,
-- and finally current response.
--

type alias Response = Maybe (Result Http.RawError Http.Response)

type alias Model =
    { path   : String
    , req    : String
    , verb   : String
    , res    : Response
    }

init : (Model, Effects Action)
init = (Model "" "" "GET" Nothing, Effects.none)

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

update : Action -> Model -> (Model, Effects Action)
update action model =
  let
    noFx m = (m, Effects.none)
  in case action of
    Path str ->
        noFx { model | path = str }
    ReqVerb m ->
        noFx { model | verb = m }
    ReqBody str ->
        noFx { model | req = str }
    ResSuccess res ->
        noFx { model | res = Just (Ok res) }
    ResError err ->
        noFx { model | res = Just (Err err) }
    Submit ->
        (model, makeRequestGiven model)

makeRequestGiven : Model -> Effects Action
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
    action =
        Http.send Http.defaultSettings request
            `andThen` (succeed << ResSuccess)
            `onError` (succeed << ResError)
  in
    Effects.task action

--
-- View: Display our app related lark:
--

view : Address Action -> Model -> Html
view address model =
    div [ class "container" ]
        [ div
            [ class "inputs" ]
            [ row "Path" "input-path" <|
                input
                    [ id "input-path", onInput address Path, value model.path ]
                    []
            , row "Method" "input-method" <|
                select
                    [ id "input-method", onInput address ReqVerb ]
                    [ option [ selected <| model.verb == "GET" ] [ text "GET" ]
                    , option [ selected <| model.verb == "POST" ] [ text "POST" ]
                    ]
            , row "JSON" "input-json" <|
                textarea [ onInput address ReqBody, value model.req ] []
            ]
        , div
            [ class "submit" ]
            [ button [ onClick address Submit ] [ text "Submit" ] ]
        , div
            [ class "response" ]
            [ viewResponse model.res ]
        ]

row : String -> String -> Html -> Html
row name id input =
    div [ class "row" ]
        [ div
            [ class "row-name"  ]
            [ label [ for id ] [ text name ] ]
        , div
            [ class "row-input" ]
            [ input ]
        ]

viewResponse : Response -> Html
viewResponse res =
  let
    noResponse =
        text "No request made"
    responseError =
        text "There was a low level error processing your request."
    response details =
        div []
            [ row "Status" ""  <| text (toString details.status ++ ": " ++ details.statusText)
            , row "Headers" "" <| div [] <| List.map viewHeader <| Dict.toList details.headers
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
            Err _   -> responseError

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (contentToValue >> Signal.message address)















