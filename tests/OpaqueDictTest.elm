module OpaqueDictTest exposing (suite)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import OpaqueDict
    exposing
        ( OpaqueDict
        , decode
        , diff
        , empty
        , encode
        , filter
        , foldl
        , foldr
        , fromList
        , get
        , insert
        , intersect
        , isEmpty
        , keys
        , map
        , member
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        , update
        , values
        )
import Test exposing (Test, describe, test)


{-| Sample animal groups.
-}
type AnimalGroup
    = Feline
    | Canine
    | Rodent


{-| Get the string form of an animal group.
-}
animalGroupToString : AnimalGroup -> String
animalGroupToString group =
    case group of
        Feline ->
            "feline"

        Canine ->
            "canine"

        Rodent ->
            "rodent"


{-| Maybe get an animal group from a string.
-}
animalGroupFromString : String -> Maybe AnimalGroup
animalGroupFromString groupName =
    case groupName of
        "feline" ->
            Just Feline

        "canine" ->
            Just Canine

        "rodent" ->
            Just Rodent

        _ ->
            Nothing


{-| A sample opaque type.
-}
type UserId
    = UserId Int


{-| A sample user.
-}
type alias User =
    { id : UserId
    , name : String
    , age : Int
    }


{-| Get the string value from the opaque type.
-}
userIdToString : UserId -> String
userIdToString (UserId id) =
    id |> String.fromInt


suite : Test
suite =
    describe "OpaqueDict"
        [ describe "empty: Make an empty dictionary."
            [ describe "when the dictionary is empty"
                [ test "it behaves as expected" <|
                    \() ->
                        empty userIdToString
                            |> Expect.equal
                                ([]
                                    |> fromList userIdToString
                                )
                ]
            ]
        , describe "singleton: Create a dictionary with one key-value pair."
            [ test "it behaves as expected" <|
                \() ->
                    singleton userIdToString (UserId 1) "Bob"
                        |> Expect.equal
                            ([ ( UserId 1, "Bob" ) ]
                                |> fromList userIdToString
                            )
            ]
        , describe "insert: Insert a key-value pair into a dictionary."
            [ describe "when there is not yet any key of that value"
                [ test "it inserts the key" <|
                    \() ->
                        []
                            |> fromList userIdToString
                            |> insert (UserId 2) "Bob"
                            |> get (UserId 2)
                            |> Expect.equal (Just "Bob")
                ]
            , describe "when there is a different value of the same key"
                [ test "it replaces the value" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> insert (UserId 2) "Bob"
                            |> get (UserId 2)
                            |> Expect.equal (Just "Bob")
                ]
            ]
        , describe "update: Update the value."
            [ describe "when there is a matching key"
                [ describe "when the value is changed"
                    [ test "it changes the value" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer maybeName =
                                    maybeName
                                        |> Maybe.map (\t -> t ++ "-Anne")
                            in
                            [ ( UserId 2, "Mary" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> get (UserId 2)
                                |> Expect.equal (Just "Mary-Anne")
                    ]
                , describe "when the mapper returns nothing"
                    [ test "it removes the value" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer =
                                    always Nothing
                            in
                            [ ( UserId 2, "Mary" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> get (UserId 2)
                                |> Expect.equal Nothing
                    ]
                ]
            , describe "when there is not a matching key"
                [ describe "when the mapper returns nothing"
                    [ test "it doesn't change the existing record" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer maybeName =
                                    maybeName
                                        |> Maybe.map (\t -> t ++ "-Anne")
                            in
                            [ ( UserId 3, "Bob" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> get (UserId 3)
                                |> Expect.equal (Just "Bob")
                    , test "it doesn't change the number of items" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer maybeName =
                                    maybeName
                                        |> Maybe.map (\t -> t ++ "-Anne")
                            in
                            [ ( UserId 3, "Bob" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> size
                                |> Expect.equal 1
                    ]
                , describe "when the mapper returns something even when the key doesn't exist"
                    [ test "it creates a new record" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer =
                                    always (Just "Anne")
                            in
                            [ ( UserId 3, "Bob" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> get (UserId 2)
                                |> Expect.equal (Just "Anne")
                    , test "it increases the number of items" <|
                        \() ->
                            let
                                transformer : Maybe String -> Maybe String
                                transformer =
                                    always (Just "Anne")
                            in
                            [ ( UserId 3, "Bob" ) ]
                                |> fromList userIdToString
                                |> update (UserId 2) transformer
                                |> size
                                |> Expect.equal 2
                    ]
                ]
            ]
        , describe "remove: Remove a value from the dictionary."
            [ describe "when there is a value matching the key"
                [ test "it removes value" <|
                    \() ->
                        [ ( UserId 3, "Bob" ) ]
                            |> fromList userIdToString
                            |> remove (UserId 3)
                            |> get (UserId 3)
                            |> Expect.equal Nothing
                ]
            , describe "when there is no value matching the key"
                [ test "it changes nothing" <|
                    \() ->
                        [ ( UserId 3, "Bob" ) ]
                            |> fromList userIdToString
                            |> remove (UserId 2)
                            |> get (UserId 3)
                            |> Expect.equal (Just "Bob")
                ]
            ]
        , describe "isEmpty: Determine whether the dictionary is empty."
            [ describe "when the dictionary is empty"
                [ test "it is true" <|
                    \() ->
                        []
                            |> fromList userIdToString
                            |> isEmpty
                            |> Expect.true "The dictionary is empty"
                ]
            , describe "when the dictionary is not empty"
                [ test "it returns false" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> isEmpty
                            |> Expect.false "The dictionary is not empty"
                ]
            ]
        , describe "member: Determine if a matching key exists."
            [ describe "when there is a matching key"
                [ test "it returns true" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> member (UserId 2)
                            |> Expect.true "Key exists"
                ]
            , describe "when there is no matching key"
                [ test "it returns false" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> member (UserId 3)
                            |> Expect.false "Key doesn't exist"
                ]
            ]
        , describe "get: Get the matching value if it exists."
            [ describe "when there is a matching value"
                [ test "it returns the value" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> get (UserId 2)
                            |> Expect.equal (Just "Mary")
                ]
            , describe "when there is no matching value"
                [ test "it returns nothing" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> get (UserId 3)
                            |> Expect.equal Nothing
                ]
            ]
        , describe "size: Get the size of the dictionary."
            [ describe "when there is one item"
                [ test "it returns 1" <|
                    \() ->
                        [ ( UserId 2, "Mary" ) ]
                            |> fromList userIdToString
                            |> size
                            |> Expect.equal 1
                ]
            , describe "when there is no matching value"
                [ test "it returns nothing" <|
                    \() ->
                        []
                            |> fromList userIdToString
                            |> size
                            |> Expect.equal 0
                ]
            , describe "when there are multiple items"
                [ test "it returns 1" <|
                    \() ->
                        [ ( UserId 2, "Mary" ), ( UserId 3, "Bob" ), ( UserId 5, "Sue" ), ( UserId 8, "Omar" ) ]
                            |> fromList userIdToString
                            |> size
                            |> Expect.equal 4
                ]
            ]
        , describe "keys: Get the keys, ordered."
            [ test "it returns the keys" <|
                \() ->
                    [ ( UserId 3, "Bob" )
                    , ( UserId 1, "Jim" )
                    , ( UserId 2, "Mary" )
                    ]
                        |> fromList userIdToString
                        |> keys
                        |> Expect.equal
                            [ UserId 1
                            , UserId 2
                            , UserId 3
                            ]
            ]
        , describe "values: Get the values."
            [ test "it returns the values" <|
                \() ->
                    [ ( UserId 3, "Bob" )
                    , ( UserId 1, "Jim" )
                    , ( UserId 2, "Mary" )
                    ]
                        |> fromList userIdToString
                        |> values
                        |> Expect.equal
                            [ "Jim"
                            , "Mary"
                            , "Bob"
                            ]
            ]
        , describe "toList: Get the list form of the dictionary."
            [ test "it returns the values in list form" <|
                \() ->
                    [ ( UserId 3, "Bob" )
                    , ( UserId 1, "Jim" )
                    , ( UserId 2, "Mary" )
                    ]
                        |> fromList userIdToString
                        |> toList
                        |> Expect.equal
                            [ ( UserId 1, "Jim" )
                            , ( UserId 2, "Mary" )
                            , ( UserId 3, "Bob" )
                            ]
            ]
        , describe "map: Apply a function to all values."
            [ test "it returns the transformed values" <|
                \() ->
                    let
                        mapper : UserId -> String -> String
                        mapper (UserId id) name =
                            if 3 > id then
                                name ++ "-Bob"

                            else
                                name
                    in
                    [ ( UserId 3, "Bob" )
                    , ( UserId 1, "Jim" )
                    , ( UserId 2, "Mary" )
                    ]
                        |> fromList userIdToString
                        |> map mapper
                        |> toList
                        |> Expect.equal
                            [ ( UserId 1, "Jim-Bob" )
                            , ( UserId 2, "Mary-Bob" )
                            , ( UserId 3, "Bob" )
                            ]
            ]
        , describe "foldl: Fold over the key-value pairs in a dictionary from lowest key to highest key."
            [ test "it returns the accumulated values" <|
                \() ->
                    let
                        getAges : OpaqueDict UserId User -> List Int
                        getAges users =
                            foldl appendAge [] users

                        appendAge : UserId -> User -> List Int -> List Int
                        appendAge _ { age } ages =
                            age :: ages
                    in
                    [ ( UserId 3, User (UserId 3) "Bob" 33 )
                    , ( UserId 1, User (UserId 1) "Jim" 28 )
                    , ( UserId 2, User (UserId 2) "Mary" 19 )
                    ]
                        |> fromList userIdToString
                        |> getAges
                        |> Expect.equal
                            [ 33, 19, 28 ]
            ]
        , describe "foldr: Fold over the key-value pairs in a dictionary from highest key to lowest key."
            [ test "it returns the accumulated values" <|
                \() ->
                    let
                        getAges : OpaqueDict UserId User -> List Int
                        getAges users =
                            foldr appendAge [] users

                        appendAge : UserId -> User -> List Int -> List Int
                        appendAge _ { age } ages =
                            age :: ages
                    in
                    [ ( UserId 3, User (UserId 3) "Bob" 33 )
                    , ( UserId 1, User (UserId 1) "Jim" 28 )
                    , ( UserId 2, User (UserId 2) "Mary" 19 )
                    ]
                        |> fromList userIdToString
                        |> getAges
                        |> Expect.equal
                            [ 28, 19, 33 ]
            ]
        , describe "filter: keep only the filtered values."
            [ test "it returns the filtered values" <|
                \() ->
                    let
                        ageFilter : UserId -> User -> Bool
                        ageFilter _ { age } =
                            age < 30
                    in
                    [ ( UserId 3, User (UserId 3) "Bob" 33 )
                    , ( UserId 1, User (UserId 1) "Jim" 28 )
                    , ( UserId 2, User (UserId 2) "Mary" 19 )
                    ]
                        |> fromList userIdToString
                        |> filter ageFilter
                        |> toList
                        |> Expect.equal
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            , ( UserId 2, User (UserId 2) "Mary" 19 )
                            ]
            ]
        , describe "partition: split into two dictionaries."
            [ test "it returns the partitioned values" <|
                \() ->
                    let
                        ageFilter : UserId -> User -> Bool
                        ageFilter _ { age } =
                            age < 30
                    in
                    [ ( UserId 3, User (UserId 3) "Bob" 33 )
                    , ( UserId 1, User (UserId 1) "Jim" 28 )
                    , ( UserId 2, User (UserId 2) "Mary" 19 )
                    ]
                        |> fromList userIdToString
                        |> partition ageFilter
                        |> Tuple.mapBoth toList toList
                        |> Expect.equal
                            ( [ ( UserId 1, User (UserId 1) "Jim" 28 )
                              , ( UserId 2, User (UserId 2) "Mary" 19 )
                              ]
                            , [ ( UserId 3, User (UserId 3) "Bob" 33 ) ]
                            )
            ]
        , describe "union: combine two dictionaries."
            [ test "it returns the combined values" <|
                \() ->
                    let
                        oneDict : OpaqueDict UserId User
                        oneDict =
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            ]
                                |> fromList userIdToString

                        anotherDict : OpaqueDict UserId User
                        anotherDict =
                            [ ( UserId 2, User (UserId 2) "Mary" 19 )
                            , ( UserId 3, User (UserId 3) "Bob" 33 )
                            ]
                                |> fromList userIdToString
                    in
                    union oneDict anotherDict
                        |> toList
                        |> Expect.equal
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            , ( UserId 2, User (UserId 2) "Mary" 19 )
                            , ( UserId 3, User (UserId 3) "Bob" 33 )
                            ]
            ]
        , describe "intersect: keep the elements common from two dictionaries."
            [ test "it returns the intersecting values" <|
                \() ->
                    let
                        oneDict : OpaqueDict UserId User
                        oneDict =
                            [ ( UserId 2, User (UserId 2) "Mary" 28 )
                            ]
                                |> fromList userIdToString

                        anotherDict : OpaqueDict UserId User
                        anotherDict =
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            , ( UserId 2, User (UserId 2) "Marianne" 19 )
                            , ( UserId 3, User (UserId 3) "Bob" 33 )
                            ]
                                |> fromList userIdToString
                    in
                    intersect oneDict anotherDict
                        |> toList
                        |> Expect.equal
                            [ ( UserId 2, User (UserId 2) "Mary" 28 )
                            ]
            ]
        , describe "diff: keep the elements not in the second dictionary."
            [ test "it returns the diff elements" <|
                \() ->
                    let
                        oneDict : OpaqueDict UserId User
                        oneDict =
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            , ( UserId 2, User (UserId 2) "Marianne" 19 )
                            , ( UserId 3, User (UserId 3) "Bob" 33 )
                            ]
                                |> fromList userIdToString

                        anotherDict : OpaqueDict UserId User
                        anotherDict =
                            [ ( UserId 2, User (UserId 2) "Mary" 28 )
                            ]
                                |> fromList userIdToString
                    in
                    diff oneDict anotherDict
                        |> toList
                        |> Expect.equal
                            [ ( UserId 1, User (UserId 1) "Jim" 28 )
                            , ( UserId 3, User (UserId 3) "Bob" 33 )
                            ]
            ]
        , describe "merge: combine two dictionaries."
            [ describe "when adding common elements"
                [ test "it returns the merged content" <|
                    \() ->
                        let
                            oneDict : OpaqueDict AnimalGroup Int
                            oneDict =
                                [ ( Rodent, 28 )
                                , ( Feline, 5 )
                                ]
                                    |> fromList animalGroupToString

                            anotherDict : OpaqueDict AnimalGroup Int
                            anotherDict =
                                [ ( Canine, 2 )
                                , ( Feline, 3 )
                                ]
                                    |> fromList animalGroupToString

                            merged : OpaqueDict AnimalGroup Int
                            merged =
                                OpaqueDict.merge
                                    (\key a -> insert key a)
                                    (\key a b -> insert key (a + b))
                                    (\key b -> insert key b)
                                    oneDict
                                    anotherDict
                                    (empty animalGroupToString)
                        in
                        merged
                            |> toList
                            |> Expect.equal
                                [ ( Canine, 2 )
                                , ( Feline, 8 )
                                , ( Rodent, 28 )
                                ]
                ]
            ]
        , describe "decode: decode a JSON object into an OpaqueDict."
            [ test "it creates the expected dictionary" <|
                \() ->
                    Decode.decodeString
                        (decode animalGroupFromString animalGroupToString Decode.int)
                        "{ \"feline\": 3, \"canine\": 5 }"
                        |> Result.map toList
                        |> Expect.equal
                            (Ok
                                [ ( Canine, 5 )
                                , ( Feline, 3 )
                                ]
                            )
            ]
        , describe "encode: encode an OpaqueDict to a JSON object."
            [ test "it creates the expected JSON" <|
                \() ->
                    let
                        animalCounts : OpaqueDict AnimalGroup Int
                        animalCounts =
                            [ ( Canine, 5 )
                            , ( Feline, 3 )
                            ]
                                |> fromList animalGroupToString
                    in
                    Encode.encode 0 (encode animalGroupToString Encode.int animalCounts)
                        |> Expect.equal
                            "{\"canine\":5,\"feline\":3}"
            ]
        ]
