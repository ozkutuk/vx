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


type alias Chord =
    { note : Note
    , accidental : Accidental
    , scale : Scale
    }


type alias Model =
    { chord : Chord
    , menuVisibility : Bool
    , triadVisibility : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Chord C Natural Major) False True
    , Random.generate NewChord randomChord
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = Change
    | NewChord Chord
    | ToggleMenu
    | ToggleTriad
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "MESSAGE: " msg of
        Change ->
            ( model
            , Random.generate NewChord randomChord
            )

        NewChord newChord ->
            ( { model | chord = newChord }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | menuVisibility = not model.menuVisibility }
            , Cmd.none
            )

        ToggleTriad ->
            ( { model | triadVisibility = not model.triadVisibility }
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

                "s" ->
                    ToggleMenu

                "t" ->
                    ToggleTriad

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


randomChord : Random.Generator Chord
randomChord =
    Random.map3 Chord note accidental scale


type alias UniqueNote =
    ( Note, Accidental )


half : UniqueNote -> UniqueNote
half ( n, a ) =
    case ( n, a ) of
        ( B, Natural ) ->
            ( C, Natural )

        ( E, Natural ) ->
            ( F, Natural )

        ( x, Natural ) ->
            ( x, Sharp )

        ( C, Flat ) ->
            ( C, Natural )

        ( F, Flat ) ->
            ( F, Natural )

        ( x, Flat ) ->
            ( x, Natural )

        ( A, Sharp ) ->
            ( B, Natural )

        ( B, Sharp ) ->
            ( C, Sharp )

        ( C, Sharp ) ->
            ( D, Natural )

        ( D, Sharp ) ->
            ( E, Natural )

        ( E, Sharp ) ->
            ( F, Sharp )

        ( F, Sharp ) ->
            ( G, Natural )

        ( G, Sharp ) ->
            ( A, Natural )


whole : UniqueNote -> UniqueNote
whole =
    half << half


iterList : a -> List (a -> a) -> List a
iterList x l =
    case l of
        [] ->
            [ x ]

        f :: fs ->
            x :: iterList (f x) fs


composeN : Int -> List (a -> a) -> (a -> a)
composeN n =
    List.foldr (<<) identity << List.take n


majorRecipe : List (UniqueNote -> UniqueNote)
majorRecipe =
    [ whole, whole, half, whole, whole, whole, half ]


major : UniqueNote -> List UniqueNote
major x =
    iterList x majorRecipe


majorTriad : UniqueNote -> ( UniqueNote, UniqueNote, UniqueNote )
majorTriad x =
    let
        fFirst =
            identity

        fThird =
            composeN 2 majorRecipe

        fFifth =
            composeN 4 majorRecipe
    in
    ( fFirst x
    , fThird x
    , fFifth x
    )


minorRecipe : List (UniqueNote -> UniqueNote)
minorRecipe =
    [ whole, half, whole, whole, half, whole, whole ]


minor : UniqueNote -> List UniqueNote
minor x =
    iterList x minorRecipe


minorTriad : UniqueNote -> ( UniqueNote, UniqueNote, UniqueNote )
minorTriad x =
    let
        fFirst =
            identity

        fThird =
            composeN 2 minorRecipe

        fFifth =
            composeN 4 minorRecipe
    in
    ( fFirst x
    , fThird x
    , fFifth x
    )


triad : Scale -> UniqueNote -> ( UniqueNote, UniqueNote, UniqueNote )
triad s =
    case s of
        Major ->
            majorTriad

        Minor ->
            minorTriad



-- VIEW


boxTextStyle : Int -> List (El.Attribute Msg)
boxTextStyle size =
    [ Font.size size
    , El.centerX
    , El.centerY
    , Font.color colors.prm
    ]


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


uniqueStr : UniqueNote -> String
uniqueStr ( n, a ) =
    noteStr n ++ accStr a


viewChord : Model -> El.Element Msg
viewChord model =
    noteStr model.chord.note
        ++ accStr model.chord.accidental
        ++ scaleStr model.chord.scale
        |> El.text
        |> El.el (boxTextStyle 80)


viewTriad : Model -> El.Element Msg
viewTriad model =
    case model.triadVisibility of
        True ->
            let
                triadStr ( x, y, z ) =
                    uniqueStr x
                        ++ " - "
                        ++ uniqueStr y
                        ++ " - "
                        ++ uniqueStr z
            in
            triad model.chord.scale ( model.chord.note, model.chord.accidental )
                |> triadStr
                |> El.text
                |> El.el (boxTextStyle 40)
                |> viewColumnBox 1

        False ->
            El.none


colors =
    { bg = El.rgb255 0x1C 0x29 0x38
    , box = El.rgb255 0xCD 0x3F 0x3E
    , prm = El.rgb255 0xF4 0xF6 0xF6
    }


boxStyle : List (El.Attribute Msg)
boxStyle =
    [ Bg.color colors.box
    , El.width El.fill
    , El.height El.fill
    , Border.rounded 50
    ]


viewRowBox : Int -> El.Element Msg -> El.Element Msg
viewRowBox portion =
    El.el <|
        boxStyle
            ++ [ El.width <| El.fillPortion portion ]


viewColumnBox : Int -> El.Element Msg -> El.Element Msg
viewColumnBox portion =
    El.el <|
        boxStyle
            ++ [ El.height <| El.fillPortion portion ]


viewMenu : Model -> El.Element Msg
viewMenu model =
    case model.menuVisibility of
        True ->
            "Settings"
                |> El.text
                |> El.el (boxTextStyle 40)
                |> viewRowBox 1

        False ->
            El.none


view : Model -> Html Msg
view model =
    El.layout [] <|
        El.el
            [ Bg.color colors.bg
            , El.paddingXY 300 150
            , El.height El.fill
            , El.width El.fill
            ]
        <|
            El.row
                [ El.width El.fill
                , El.height El.fill
                , El.spacing 50
                ]
                [ El.column
                    [ El.centerX
                    , El.centerY
                    , El.width <| El.fillPortion 4
                    , El.height El.fill
                    , El.spacing 50
                    ]
                    [ viewColumnBox 8 (viewChord model)
                    , viewTriad model
                    ]
                , viewMenu model
                ]
