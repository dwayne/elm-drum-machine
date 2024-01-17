module Timer exposing
    ( Id
    , Timer
    , apply
    , new
    , setAlarm
    )

import Process
import Task


type Timer
    = Timer Id


type Id
    = Id Int


new : Timer
new =
    Timer <| Id 0


setAlarm : Float -> (Id -> msg) -> Timer -> ( Timer, Cmd msg )
setAlarm duration onTimeUp (Timer id) =
    let
        nextId =
            increment id
    in
    ( Timer nextId
    , Process.sleep duration
        |> Task.perform (always <| onTimeUp nextId)
    )


increment : Id -> Id
increment (Id n) =
    Id <| n + 1


apply : Id -> Timer -> (() -> a) -> Maybe a
apply previousId (Timer latestId) f =
    if previousId == latestId then
        Just <| f ()

    else
        Nothing
