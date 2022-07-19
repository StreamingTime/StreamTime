port module LocalStorage exposing (PersistentData, decodePersistentData, encode, persistToken)

import Json.Decode as Decode
import Json.Encode as Encode
import Twitch


type alias PersistentData =
    { token : String
    }


port setStorage : Encode.Value -> Cmd msg


encode : PersistentData -> Encode.Value
encode data =
    Encode.object
        [ ( "token", Encode.string data.token )
        ]


persistToken : Twitch.Token -> Cmd msg
persistToken (Twitch.Token value) =
    setStorage (encode { token = value })


decoder : Decode.Decoder PersistentData
decoder =
    Decode.map PersistentData
        (Decode.field "token" Decode.string)


decodePersistentData : Encode.Value -> Result Decode.Error PersistentData
decodePersistentData data =
    Decode.decodeValue decoder data
