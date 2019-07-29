port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (attribute, class, tabindex)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode as E
import Time


port selectTopic : String -> Cmd msg


port recenterTopic : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pendingRecenter of
        Waiting _ ->
            Time.every 400 Held

        _ ->
            Sub.none


type alias Topic =
    { label : String
    , description : String
    , url : String
    }


type alias Index =
    Int


type TopicType
    = Prerequisite
    | PostRequisite
    | Current


type alias TopicConfig =
    { label : String
    , colorClass : String
    }


getConfig : TopicType -> TopicConfig
getConfig topicType =
    case topicType of
        Prerequisite ->
            TopicConfig "Prerequisites" "pre"

        Current ->
            TopicConfig "Current Topic" "current"

        PostRequisite ->
            TopicConfig "Postrequisites" "post"


type alias TopicGraph =
    { topic : Topic
    , pre : List Topic
    , post : List Topic
    }


type alias Flags =
    { graph : TopicGraph
    , selected : Topic
    }


type alias Coords =
    { x : Float
    , y : Float
    }


type PendingRecenter
    = NotWaiting
    | Waiting Topic
    | Recentered


type alias Model =
    { graph : TopicGraph
    , selected : Topic
    , pendingRecenter : PendingRecenter
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.graph flags.selected NotWaiting, Cmd.none )


type Msg
    = Wait Topic
    | ReleasedEarly
    | Held Time.Posix
    | SelectTopic Topic


view : Model -> Html Msg
view model =
    let
        currentIndex =
            List.length model.graph.pre

        postIndex =
            currentIndex + 1
    in
    div [ class "outer" ]
        [ div [ class "layout", attribute "role" "nav" ]
            [ renderList Prerequisite model.graph.pre model.selected 0
            , renderList Current [ model.graph.topic ] model.selected currentIndex
            , renderList PostRequisite model.graph.post model.selected postIndex
            ]
        , renderHelp
        ]


renderHelp : Html Msg
renderHelp =
    div [ class "help" ]
        [ text "Click to view a topic, click and hold to view and recenter"
        ]


renderListItem : TopicType -> Topic -> Int -> Topic -> Html Msg
renderListItem topicType selected index topic =
    let
        classes =
            "list-group-item "
                ++ (getConfig topicType |> .colorClass)
                ++ (if selected == topic then
                        " selected"

                    else
                        " unselected"
                   )
    in
    li
        [ class classes
        , attribute "role" "button"
        , tabindex index
        , Html.Events.onClick (SelectTopic topic)
        , Html.Events.onMouseDown (Wait topic)
        , Html.Events.onMouseUp ReleasedEarly
        , Html.Events.on "keypress" (Json.Decode.succeed (SelectTopic topic))
        ]
        [ Html.text topic.label ]


renderList : TopicType -> List Topic -> Topic -> Int -> Html Msg
renderList topicType contents selected tabIndex =
    let
        listItems =
            List.indexedMap (\i t -> renderListItem topicType selected (i + tabIndex) t) contents
    in
    ul [ class "list-group layout-child" ] listItems


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Wait topic ->
            ( { model | pendingRecenter = Waiting topic }, Cmd.none )

        ReleasedEarly ->
            case model.pendingRecenter of
                Waiting _ ->
                    ( { model | pendingRecenter = NotWaiting }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Held _ ->
            case model.pendingRecenter of
                Waiting topic ->
                    ( { model | selected = topic, pendingRecenter = Recentered }, recenterTopic topic.url )

                _ ->
                    ( model, Cmd.none )

        SelectTopic topic ->
            case model.pendingRecenter of
                NotWaiting ->
                    ( { model | selected = topic }, selectTopic topic.url )

                _ ->
                    ( model, Cmd.none )
