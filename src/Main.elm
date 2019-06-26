module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (attribute, tabindex)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import VirtualDom


radius =
    40.0


xGap =
    100.0


defaultWidth =
    400.0


defaultHeight =
    400.0


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
    TopicGraph


type alias Coords =
    { x : Float
    , y : Float
    }


type alias Model =
    { graph : TopicGraph
    , selected : Topic
    , zoomFactor : Float
    , viewCoords : Coords
    , lastPan : Maybe Coords
    , isPanning : Bool
    }


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    }


decoder : Json.Decode.Decoder MouseMoveData
decoder =
    map2 MouseMoveData
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)


init : Flags -> ( Model, Cmd Msg )
init graph =
    ( Model graph graph.topic 1.0 (Coords -200.0 -200.0) Nothing False, Cmd.none )


type Msg
    = SelectTopic Topic
    | StartPan
    | EndPan
    | Pan MouseMoveData
    | ZoomIn
    | ZoomOut


renderAllTopics : TopicGraph -> Topic -> List (Html Msg)
renderAllTopics graph selected =
    let
        preCount =
            List.length graph.pre

        pres =
            List.indexedMap (\i t -> renderTopic t (t == selected) Prerequisite i preCount) graph.pre

        postCount =
            List.length graph.post

        toPost =
            \i t -> renderTopic t (t == selected) PostRequisite i postCount

        posts =
            List.indexedMap toPost graph.post

        topic =
            renderTopicAtPosition graph.topic "green" (graph.topic == selected) 0.0 0.0
    in
    pres ++ [ topic ] ++ posts


renderText : String -> Float -> Float -> List (Html Msg)
renderText label xPos yPos =
    let
        possibleTextLength =
            if String.length label > 10 then
                [ textLength (String.fromFloat (radius * 2)) ]

            else
                []
    in
    [ text_
        ([ x (String.fromFloat xPos)
         , y (String.fromFloat (yPos + 3))
         , lengthAdjust "spacingAndGlyphs"
         , textAnchor "middle"
         , fontFamily "Arial, Helvetica, sans-serif"
         ]
            ++ possibleTextLength
        )
        [ Svg.text label ]
    ]


renderTopicAtPosition : Topic -> String -> Bool -> Float -> Float -> Html Msg
renderTopicAtPosition topic color isSelected x y =
    let
        strokeColor =
            if isSelected then
                "red"

            else
                "white"
    in
    g []
        ([ circle
            [ cx (String.fromFloat x)
            , cy (String.fromFloat y)
            , r (String.fromFloat radius)
            , fill color
            , stroke strokeColor
            , strokeWidth "2"
            , Svg.Events.onClick (SelectTopic topic)
            ]
            []
         ]
            ++ renderText topic.label x y
        )


renderTopic : Topic -> Bool -> TopicType -> Int -> Int -> Html Msg
renderTopic topic isSelected relationship index total =
    let
        ( y, color ) =
            case relationship of
                Prerequisite ->
                    ( -100.0, "orange" )

                PostRequisite ->
                    ( 100.0, "blue" )

                Current ->
                    ( 0.0, "green" )

        isOdd =
            modBy 2 total == 1

        anchorIndex =
            if isOdd then
                (total - 1) // 2

            else
                (total // 2) - 1

        xOffset =
            if isOdd then
                0

            else
                xGap / 2

        x =
            if index > anchorIndex then
                (Basics.toFloat (index - anchorIndex) * xGap) - xOffset

            else
                (-1.0 * (Basics.toFloat (anchorIndex - index) * xGap)) - xOffset
    in
    renderTopicAtPosition topic color isSelected x y


options =
    { preventDefault = True, stopPropagation = True }


loggingDecoder : Decoder a -> Decoder a
loggingDecoder realDecoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\value ->
                case Json.Decode.decodeValue realDecoder value of
                    Ok decoded ->
                        Json.Decode.succeed decoded

                    Err error ->
                        Json.Decode.fail <| Debug.log "decode error" <| Json.Decode.errorToString error
            )


renderZoomControls : Model -> Html Msg
renderZoomControls model =
    let
        normalize =
            \v -> String.fromFloat (v * model.zoomFactor)

        zoomRectAttrs =
            [ width <| normalize 10
            , height <| normalize 10
            , strokeWidth <| normalize 1
            , stroke "gray"
            , fillOpacity "0"
            ]
    in
    g
        []
        [ rect
            (zoomRectAttrs
                ++ [ Svg.Events.onClick ZoomIn
                   , x <| String.fromFloat (5 * model.zoomFactor + model.viewCoords.x)
                   , y <| String.fromFloat (5 * model.zoomFactor + model.viewCoords.y)
                   ]
            )
            []
        , rect
            (zoomRectAttrs
                ++ [ Svg.Events.onClick ZoomOut
                   , x <| String.fromFloat (5 * model.zoomFactor + model.viewCoords.x)
                   , y <| String.fromFloat (15 * model.zoomFactor + model.viewCoords.y)
                   ]
            )
            []
        ]


view : Model -> Html Msg
view model =
    viewPrototype2 model


viewPrototype2 : Model -> Html Msg
viewPrototype2 model =
    let
        currentIndex =
            List.length model.graph.pre

        postIndex =
            currentIndex + 1
    in
    div [ class "layout", attribute "role" "button" ]
        [ renderList Prerequisite model.graph.pre model.selected 0
        , renderList Current [ model.graph.topic ] model.selected currentIndex
        , renderList PostRequisite model.graph.post model.selected postIndex
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


viewPrototype1 : Model -> Html Msg
viewPrototype1 model =
    let
        vb =
            String.fromFloat model.viewCoords.x ++ " " ++ String.fromFloat model.viewCoords.y ++ " " ++ String.fromFloat (defaultWidth * model.zoomFactor) ++ " " ++ String.fromFloat (defaultHeight * model.zoomFactor)

        possibleMouseListener =
            if model.isPanning then
                [ VirtualDom.on "mousemove" <|
                    VirtualDom.Normal <|
                        Json.Decode.map Pan (loggingDecoder decoder)
                ]

            else
                []
    in
    svg
        ([ viewBox vb
         , height "400"
         , width "400"
         , Svg.Events.onMouseDown StartPan
         , Svg.Events.onMouseUp EndPan
         , fill "white"
         ]
            ++ possibleMouseListener
        )
        [ g []
            [ rect
                [ x (String.fromFloat model.viewCoords.x)
                , y (String.fromFloat model.viewCoords.y)
                , fill "white"
                , width "100%"
                , height "100%"
                ]
                []
            ]
        , g [] (renderAllTopics model.graph model.selected)
        , g [] [ renderZoomControls model ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTopic topic ->
            ( { model | selected = topic }, Cmd.none )

        ZoomIn ->
            ( { model | zoomFactor = model.zoomFactor / 1.2 }, Cmd.none )

        ZoomOut ->
            ( { model | zoomFactor = model.zoomFactor * 1.2 }, Cmd.none )

        StartPan ->
            ( { model | isPanning = True }, Cmd.none )

        EndPan ->
            ( { model | isPanning = False }, Cmd.none )

        Pan data ->
            case model.lastPan of
                Just coords ->
                    let
                        lastPan =
                            Coords (Basics.toFloat data.offsetX) (Basics.toFloat data.offsetY)

                        viewCoords =
                            Coords
                                (model.viewCoords.x + (coords.x - Basics.toFloat data.offsetX))
                                (model.viewCoords.y + (coords.y - Basics.toFloat data.offsetY))
                    in
                    ( { model | lastPan = Just lastPan, viewCoords = viewCoords }, Cmd.none )

                Nothing ->
                    let
                        lastPan =
                            Coords (Basics.toFloat data.offsetX) (Basics.toFloat data.offsetY)
                    in
                    ( { model | lastPan = Just lastPan }, Cmd.none )
