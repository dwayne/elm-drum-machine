port module Main exposing (main)

import Bank exposing (Bank, KeyConfig, Kit)
import Browser
import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Key exposing (Key)
import Lib.Json.Decode as JD
import Timer exposing (Timer)
import Volume exposing (Volume)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    JE.Value


type alias Model =
    Maybe State


type alias State =
    { bank : Bank
    , isOn : Bool
    , text : String
    , volume : Volume
    , activeKey : Maybe Key
    , timer : Timer
    }


init : Flags -> ( Model, Cmd msg )
init value =
    let
        model =
            case JD.decodeValue Bank.decoder value of
                Ok bank ->
                    Just
                        { bank = bank
                        , isOn = False
                        , text = ""
                        , volume = Volume.fromInt 50
                        , activeKey = Nothing
                        , timer = Timer.init
                        }

                Err _ ->
                    Nothing
    in
    ( model
    , Cmd.none
    )


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 500
        , onExpire = DisplayTimeUp
        , onChange = ChangedTimer
        }



-- UPDATE


type Msg
    = ToggledPower
    | ToggledBank
    | ChangedVolume Volume
    | MouseDownOnKey KeyConfig
    | KeyDown KeyConfig
    | KeyUp Key
    | DisplayTimeUp
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Just state ->
            if state.isOn then
                updateOn msg state
                    |> Tuple.mapFirst Just

            else
                updateOff msg state
                    |> Tuple.mapFirst Just

        Nothing ->
            ( model
            , Cmd.none
            )


updateOn : Msg -> State -> ( State, Cmd Msg )
updateOn msg state =
    case msg of
        ToggledPower ->
            { state | isOn = False }
                |> setDisplay "Off"

        ToggledBank ->
            let
                bank =
                    Bank.switch state.bank

                kit =
                    Bank.kit bank
            in
            { state | bank = bank }
                |> setDisplay kit.name

        ChangedVolume volume ->
            { state | volume = volume }
                |> setDisplay ("Volume " ++ Volume.toString volume)

        MouseDownOnKey { id, name } ->
            setDisplay name state
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , play id state.volume
                            ]
                    )

        KeyDown { id, name, key } ->
            if state.activeKey == Nothing then
                setDisplay name { state | activeKey = Just key }
                    |> Tuple.mapSecond
                        (\cmd ->
                            Cmd.batch
                                [ cmd
                                , play id state.volume
                                ]
                        )

            else
                ( state
                , Cmd.none
                )

        KeyUp key ->
            ( if Just key == state.activeKey then
                { state | activeKey = Nothing }

              else
                state
            , Cmd.none
            )

        DisplayTimeUp ->
            clearDisplay state

        ChangedTimer timerMsg ->
            ( state
            , Timer.update timerConfig timerMsg state.timer
            )


updateOff : Msg -> State -> ( State, Cmd Msg )
updateOff msg state =
    case msg of
        ToggledPower ->
            { state | isOn = True }
                |> setDisplay "On"

        DisplayTimeUp ->
            clearDisplay state

        _ ->
            ( state
            , Cmd.none
            )


setDisplay : String -> State -> ( State, Cmd Msg )
setDisplay text state =
    let
        ( timer, cmd ) =
            Timer.setTimeout timerConfig state.timer
    in
    ( { state | text = text, timer = timer }
    , cmd
    )


clearDisplay : State -> ( State, Cmd Msg )
clearDisplay state =
    ( { state | text = "" }
    , Cmd.none
    )


play : String -> Volume -> Cmd Msg
play id volume =
    let
        message =
            JE.object
                [ ( "tag", JE.string "play" )
                , ( "value"
                  , JE.object
                        [ ( "id", JE.string id )
                        , ( "volume", Volume.toValue volume )
                        ]
                  )
                ]
    in
    send message



-- PORT


port send : JE.Value -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Just { bank, isOn } ->
            if isOn then
                Sub.batch
                    [ BE.onKeyDown <| JD.map KeyDown (keyConfigDecoder bank)
                    , BE.onKeyUp <| JD.map KeyUp keyDecoder
                    ]

            else
                Sub.none

        Nothing ->
            Sub.none


keyConfigDecoder : Bank -> JD.Decoder KeyConfig
keyConfigDecoder bank =
    let
        findKeyConfig key =
            Bank.findKeyConfig key bank
    in
    JD.field "key" JD.string
        |> JD.andThen (Key.fromString >> Maybe.andThen findKeyConfig >> JD.fromMaybe)


keyDecoder : JD.Decoder Key
keyDecoder =
    JD.field "key" JD.string
        |> JD.andThen (Key.fromString >> JD.fromMaybe)



-- VIEW


view : Model -> H.Html Msg
view model =
    case model of
        Just { bank, isOn, text, volume, activeKey } ->
            viewLayout <| viewDrumMachine bank isOn text volume activeKey

        Nothing ->
            viewError "Sorry, we're unable to start the application since it's not properly configured."


viewLayout : H.Html msg -> H.Html msg
viewLayout html =
    H.div [ HA.class "layout" ]
        [ H.div [ HA.class "layout__content" ]
            [ html ]
        ]


viewDrumMachine : Bank -> Bool -> String -> Volume -> Maybe Key -> H.Html Msg
viewDrumMachine bank isOn text volume activeKey =
    let
        isDisabled =
            not isOn
    in
    H.div [ HA.class "drum-machine" ]
        [ H.div [ HA.class "drum-machine__panel" ]
            [ viewPanel isDisabled activeKey MouseDownOnKey <| Bank.kit bank ]
        , H.div [ HA.class "drum-machine__controls" ]
            [ H.div [ HA.class "drum-machine__power" ]
                [ viewPower isOn ]
            , H.div [ HA.class "drum-machine__display" ]
                [ viewDisplay text ]
            , H.div [ HA.class "drum-machine__volume" ]
                [ Volume.view ChangedVolume isDisabled volume ]
            , H.div [ HA.class "drum-machine__bank" ]
                [ viewBank isDisabled <| Bank.isBank2 bank ]
            ]
        ]


viewPanel : Bool -> Maybe Key -> (KeyConfig -> msg) -> Kit -> H.Html msg
viewPanel isDisabled activeKey onMouseDown kit =
    H.div [ HA.class "panel" ]
        [ H.div [ HA.class "panel__container" ] <|
            List.indexedMap (viewPanelSpot isDisabled activeKey onMouseDown) kit.keyConfigs
        ]


viewPanelSpot : Bool -> Maybe Key -> (KeyConfig -> msg) -> Int -> KeyConfig -> H.Html msg
viewPanelSpot isDisabled activeKey onMouseDown index keyConfig =
    let
        ( r, c ) =
            ( index // 3 + 1, modBy 3 index + 1 )
    in
    H.div
        [ HA.class "panel__spot"
        , HA.class <| "r" ++ String.fromInt r
        , HA.class <| "c" ++ String.fromInt c
        ]
        [ viewKey isDisabled activeKey (onMouseDown keyConfig) keyConfig.key ]


viewKey : Bool -> Maybe Key -> msg -> Key -> H.Html msg
viewKey isDisabled activeKey onMouseDown key =
    H.button
        [ HA.class "key"
        , HA.classList [ ( "key--is-active", Just key == activeKey ) ]
        , HA.disabled isDisabled
        , HE.onMouseDown onMouseDown
        ]
        [ H.text <| Key.toString key ]


viewPower : Bool -> H.Html Msg
viewPower isOn =
    viewLabelledSwitch False ToggledPower isOn "Power"


viewBank : Bool -> Bool -> H.Html Msg
viewBank isDisabled isOn =
    viewLabelledSwitch isDisabled ToggledBank isOn "Bank"


viewLabelledSwitch : Bool -> msg -> Bool -> String -> H.Html msg
viewLabelledSwitch isDisabled onToggle isOn title =
    H.label [ HA.class "labelled-switch" ]
        [ H.span [ HA.class "labelled-switch__title" ] [ H.text title ]
        , viewSwitch isDisabled onToggle isOn
        ]


viewSwitch : Bool -> msg -> Bool -> H.Html msg
viewSwitch isDisabled onToggle isOn =
    H.input
        [ HA.type_ "checkbox"
        , HA.class "switch"
        , HA.disabled isDisabled
        , HA.checked isOn
        , HE.onCheck (always onToggle)
        ]
        []


viewDisplay : String -> H.Html msg
viewDisplay text =
    H.div [ HA.class "display" ]
        [ H.text text ]


viewError : String -> H.Html msg
viewError text =
    H.div
        [ HA.class "error" ]
        [ H.text text ]
