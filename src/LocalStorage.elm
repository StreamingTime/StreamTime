port module LocalStorage exposing (PersistentData, decodePersistentData, encode, persistData)

import Json.Decode as Decode
import Json.Encode as Encode


type alias PersistentData =
    { token : String
    }


port setStorage : Encode.Value -> Cmd msg


encode : PersistentData -> Encode.Value
encode data =
    Encode.object
        [ ( "token", Encode.string data.token )
        ]


decoder : Decode.Decoder PersistentData
decoder =
    Decode.map PersistentData
        (Decode.field "token" Decode.string)


persistData : PersistentData -> Cmd msg
persistData data =
    setStorage (encode data)


decodePersistentData : Encode.Value -> Result Decode.Error PersistentData
decodePersistentData data =
    Decode.decodeValue decoder data
