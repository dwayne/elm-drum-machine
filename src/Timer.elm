module Timer exposing
    ( Config
    , Msg
    , Timer
    , cancel
    , config
    , init
    , setTimeout
    , update
    )

import Process
import Task


type Timer
    = Timer Int


init : Timer
init =
    Timer 0


type Config msg
    = Config
        { wait : Int
        , onExpire : msg
        , onChange : Msg -> msg
        }


config :
    { wait : Int
    , onExpire : msg
    , onChange : Msg -> msg
    }
    -> Config msg
config { wait, onExpire, onChange } =
    Config
        { wait = max 0 wait
        , onExpire = onExpire
        , onChange = onChange
        }


setTimeout : Config msg -> Timer -> ( Timer, Cmd msg )
setTimeout (Config c) (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , Timeout newId
        |> sleep c.wait
        |> Cmd.map c.onChange
    )


cancel : Timer -> Timer
cancel (Timer id) =
    Timer <| id + 1


type Msg
    = Timeout Int


update : Config msg -> Msg -> Timer -> Cmd msg
update (Config c) (Timeout incomingId) (Timer id) =
    if incomingId == id then
        dispatch c.onExpire

    else
        Cmd.none


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
