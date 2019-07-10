module Main exposing (Model, Msg(..), init, main)

import Browser
import Browser.Events exposing (onKeyDown)
import Debug exposing (toString)
import Element as El
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
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


boxTextStyle : Int -> List (El.Attribute Msg)
boxTextStyle size =
    [ Font.size size
    , El.centerX
    , El.centerY
    , Font.color colors.prm
    ]


viewChord : Model -> El.Element Msg
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
        |> El.text
        |> El.el (boxTextStyle 80)


colors =
    { bg = El.rgb255 0x1C 0x29 0x38
    , box = El.rgb255 0xCD 0x3F 0x3E
    , prm = El.rgb255 0xF4 0xF6 0xF6
    }


view : Model -> Html Msg
view model =
    let
        r1 =
            El.el
                [ Bg.color colors.box
                , El.width El.fill
                , El.height <| El.fillPortion 8
                , Border.rounded 50
                ]
                (viewChord model)

        r2 =
            El.el
                [ Bg.color colors.box
                , El.width El.fill
                , El.height <| El.fillPortion 1
                , Border.rounded 50
                ]
                (El.el (boxTextStyle 40) <| El.text "bok")
    in
    El.layout [] <|
        El.el
            [ Bg.color colors.bg
            , El.paddingXY 300 150
            , El.height El.fill
            , El.width El.fill
            ]
        <|
            El.column
                [ El.centerX
                , El.centerY
                , El.width El.fill
                , El.height El.fill
                , El.spacing 50
                ]
                [ r1
                ]
