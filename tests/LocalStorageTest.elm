module LocalStorageTest exposing (decodePersistentDataTest, encodeTest)

import Expect
import Json.Encode as Encode
import LocalStorage
import Test exposing (Test, test)


decodePersistentDataTest : Test
decodePersistentDataTest =
    test "test decode persistent data"
        (\_ ->
            let
                object =
                    Encode.object
                        [ ( "token", Encode.string "123" ) ]
            in
            Expect.equal
                (Ok { token = "123" })
                (LocalStorage.decodePersistentData object)
        )


encodeTest : Test
encodeTest =
    test "test encode"
        (\_ ->
            let
                data =
                    { token = "123" }
            in
            Expect.equal
                (Encode.object
                    [ ( "token", Encode.string "123" ) ]
                )
                (LocalStorage.encode data)
        )
