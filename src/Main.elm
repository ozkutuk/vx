module Main exposing (Model, Msg(..), init, main)

import Browser
import Browser.Events exposing (onKeyDown)
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Random


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Note
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Accidental
    = Flat
    | Sharp
    | Natural


type Scale
    = Minor
    | Major


type alias Model =
    { note : Note
    , accidental : Accidental
    , scale : Scale
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model C Natural Major
    , Random.generate NewChord randomChord
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = Change
    | NewChord Model
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change ->
            ( model
            , Random.generate NewChord randomChord
            )

        NewChord newChord ->
            ( newChord
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    let
        select : String -> Msg
        select key =
            case key of
                " " ->
                    Change

                _ ->
                    NoOp
    in
    Json.Decode.map select (Json.Decode.field "key" Json.Decode.string)


note : Random.Generator Note
note =
    Random.uniform C [ D, E, F, G, A, B ]


accidental : Random.Generator Accidental
accidental =
    Random.uniform Sharp [ Flat, Natural ]


scale : Random.Generator Scale
scale =
    Random.uniform Major [ Minor ]


randomChord : Random.Generator Model
randomChord =
    Random.map3 Model note accidental scale



-- VIEW


viewChord : Model -> Html Msg
viewChord model =
    let
        noteStr : Note -> String
        noteStr x =
            case x of
                C ->
                    "C"

                D ->
                    "D"

                E ->
                    "E"

                F ->
                    "F"

                G ->
                    "G"

                A ->
                    "A"

                B ->
                    "B"

        accStr : Accidental -> String
        accStr x =
            case x of
                Sharp ->
                    "♯"

                Flat ->
                    "♭"

                Natural ->
                    ""

        scaleStr : Scale -> String
        scaleStr x =
            case x of
                Major ->
                    ""

                Minor ->
                    "m"
    in
    noteStr model.note
        ++ accStr model.accidental
        ++ scaleStr model.scale
        |> text


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ viewChord model ]
        ]
