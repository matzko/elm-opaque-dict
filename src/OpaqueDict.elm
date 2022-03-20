module OpaqueDict exposing
    ( OpaqueDict
    , empty
    , singleton
    , insert
    , update
    , remove
    , isEmpty
    , member
    , get
    , size
    , keys
    , values
    , toList
    , fromList
    , map
    , foldl
    , foldr
    , filter
    , partition
    , union
    , intersect
    , diff
    , merge
    , decode
    , encode
    )

{-| A dictionary mapping unique keys to values. The keys can be any type that can be transformed to a String.


# Dictionaries

@docs OpaqueDict


# Build

@docs empty
@docs singleton
@docs insert
@docs update
@docs remove


# Query

@docs isEmpty
@docs member
@docs get
@docs size


# Lists

@docs keys
@docs values
@docs toList
@docs fromList


# Transform

@docs map
@docs foldl
@docs foldr
@docs filter
@docs partition
@docs union
@docs intersect
@docs diff
@docs merge


# Miscellaneous

@docs decode
@docs encode

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A dictionary of keys and values. So a `OpaqueDict UserId User` is a dictionary
that lets you look up a `UserId` (such as a user Id) and find the associated
`User`.
import OpaqueDict exposing (OpaqueDict)

    users : OpaqueDict UserId User
    users =
        OpaqueDict.fromList userIdToString
            [ ( UserId 1, User (UserId 1) "Alice" 28 1.65 )
            , ( UserId 2, User (UserId 2) "Bob" 19 1.82 )
            , ( UserId 3, User (UserId 3) "Chuck" 33 1.75 )
            ]

    type UserId
        = UserId Int

    userIdToString : UserId -> String
    userIdToString (UserId id) =
        id |> String.fromInt

    type alias User =
        { id : UserId
        , name : String
        , age : Int
        , height : Float
        }

-}
type alias OpaqueDict key b =
    { keyToString : key -> String
    , dict : Dict String ( key, b )
    }


{-| Decode a JSON object to an OpaqueDict, analogous Json.Decode.dict.

    decodeString (decode animalGroupFromString animalGroupToString int) "{ \"feline\": 3, \"canine\": 5 }"
        == Ok (OpaqueDict.fromList animalGroupToString [ ( Feline, 3 ), ( Canine, 5 ) ])

If you need the keys (like `"feline"` and `"canine"`) available in the `OpaqueDict`
values as well, you can use a (private) intermediate data structure like
`Info` in this example:

    import OpaqueDict exposing (OpaqueDict)
    import Json.Decode exposing (..)

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

    decoder : Decoder (OpaqueDict AnimalGroup Int)
    decoder =
        Json.Decode.map (OpaqueDict.map infoToUser) (OpaqueDict.decode Json.Decode.int)


    decodeString (decode animalGroupFromString animalGroupToString int) "{ \"feline\": 3, \"canine\": 5 }"
        == Ok (OpaqueDict.fromList animalGroupToString [ ( Feline, 3 ), ( Canine, 5 ) ])

-}
decode : (String -> Maybe key) -> (key -> String) -> Decoder b -> Decoder (OpaqueDict key b)
decode stringToKey keyToString decoder =
    decoder
        |> Decode.dict
        |> Decode.andThen
            (\dict ->
                dict
                    |> Dict.toList
                    |> List.filterMap
                        (\( k, v ) ->
                            case stringToKey k of
                                Just opaqueKey ->
                                    Just ( opaqueKey, v )

                                _ ->
                                    Nothing
                        )
                    |> fromList keyToString
                    |> Decode.succeed
            )


{-| Turn an OpaqueDict into a JSON object. Analogous to Json.Encode.dict.

    animalCounts : OpaqueDict AnimalGroup Int
    animalCounts =
        [ ( Canine, 5 )
        , ( Feline, 3 )
        ]
            |> fromList animalGroupToString

    Encode.encode 0 (OpaqueDict.encode animalGroupToString Encode.int animalCounts)
        == "{\"canine\":5,\"feline\":3}"

-}
encode : (k -> String) -> (v -> Encode.Value) -> OpaqueDict k v -> Encode.Value
encode toKey toValue =
    toList
        >> List.map (Tuple.mapBoth toKey toValue)
        >> Encode.object


{-| Create an empty opaque dictionary providing a function that transforms a key type to a string.


    type UserId
        = UserId Int

    userIdToString : UserId -> String
    userIdToString (UserId id) =
        id |> String.fromInt

    -- OpaqueDict.empty userIdToString == [] OpaqueDict.fromList userIdToString

-}
empty : (key -> String) -> OpaqueDict key val
empty f =
    { keyToString = f
    , dict = Dict.empty
    }


{-| Create a dictionary with one key-value pair.

    singleton userIdToString (UserId 1) "Bob"
    -- == ([ ( UserId 1, "Bob" ) ]
    -- |> fromList userIdToString
    -- )

-}
singleton : (key -> String) -> key -> val -> OpaqueDict key val
singleton f key val =
    { keyToString = f
    , dict = [ ( key |> f, ( key, val ) ) ] |> Dict.fromList
    }


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (a -> b -> Bool) -> OpaqueDict a b -> OpaqueDict a b
filter f { keyToString, dict } =
    dict
        |> Dict.values
        |> List.filterMap
            (\( k, v ) ->
                if f k v then
                    Just ( k, v )

                else
                    Nothing
            )
        |> fromList keyToString


{-| Get all of the keys in a dictionary, sorted from lowest to highest by their string form.
-}
keys : OpaqueDict a b -> List a
keys { keyToString, dict } =
    dict
        |> Dict.values
        |> List.map (\( k, _ ) -> ( k |> keyToString, k ))
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


{-| Determine whether the opaque dictionary is empty.
-}
isEmpty : OpaqueDict key val -> Bool
isEmpty { dict } =
    dict |> Dict.isEmpty


{-| Determine if a key is in a dictionary.
-}
member : key -> OpaqueDict key val -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        _ ->
            False


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList animalFamilyToString [ (Feline, Cat), (Rodent, Mouse) ]
    get Feline animals == Just Cat
    get Rodent animals == Just Mouse
    get Canine animals == Nothing

-}
get : key -> OpaqueDict key val -> Maybe val
get key { keyToString, dict } =
    dict
        |> Dict.get (key |> keyToString)
        |> Maybe.map Tuple.second


{-| Insert a key-value pair into a dictionary. Replaces value when there is a collision.
-}
insert : a -> b -> OpaqueDict a b -> OpaqueDict a b
insert key value ({ keyToString, dict } as opaque) =
    { opaque
        | dict =
            dict
                |> Dict.insert (key |> keyToString) ( key, value )
    }


{-| Apply a function to all values in a dictionary.
-}
map : (key -> a -> b) -> OpaqueDict key a -> OpaqueDict key b
map f { keyToString, dict } =
    let
        mapper : c -> ( key, a ) -> ( key, b )
        mapper _ ( originalKey, val ) =
            ( originalKey, f originalKey val )
    in
    { keyToString = keyToString
    , dict =
        dict |> Dict.map mapper
    }


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.


    getAges : OpaqueDict UserId User -> List Int
    getAges users =
        OpaqueDict.foldl addAge [] users

    addAge : UserId -> User -> List UserId -> List Int
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (k -> v -> b -> b) -> b -> OpaqueDict k v -> b
foldl =
    folder Dict.foldl


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.


    getAges : OpaqueDict UserId User -> List Int
    getAges users =
        OpaqueDict.foldr addAge [] users

    addAge : UserId -> User -> List UserId -> List Int
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (k -> v -> b -> b) -> b -> OpaqueDict k v -> b
foldr =
    folder Dict.foldr


{-| Internal function, for performing dictionary-folding.
-}
folder :
    ((String -> ( k, v ) -> b -> b)
     -> b
     -> Dict String ( k, v )
     -> b
    )
    -> (k -> v -> b -> b)
    -> b
    -> OpaqueDict k v
    -> b
folder coreDictF f acc { dict } =
    let
        func : String -> ( k, v ) -> b -> b
        func _ ( originalKey, val ) accumulated =
            f originalKey val accumulated
    in
    coreDictF func acc dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (key -> val -> Bool) -> OpaqueDict key val -> ( OpaqueDict key val, OpaqueDict key val )
partition isGood { dict, keyToString } =
    let
        func : String -> ( key, val ) -> Bool
        func _ ( key, val ) =
            isGood key val

        dictToOpaque : Dict String ( key, val ) -> OpaqueDict key val
        dictToOpaque classic =
            { keyToString = keyToString, dict = classic }
    in
    Dict.partition func dict
        |> Tuple.mapBoth dictToOpaque dictToOpaque


{-| Remove a key-value pair from a dictionary. If the key is not found, no changes are made.
-}
remove : key -> OpaqueDict key val -> OpaqueDict key val
remove key ({ keyToString, dict } as opaque) =
    { opaque
        | dict =
            dict |> Dict.remove (key |> keyToString)
    }


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : a -> (Maybe b -> Maybe b) -> OpaqueDict a b -> OpaqueDict a b
update key f ({ keyToString, dict } as opaque) =
    let
        mapper : Maybe ( a, b ) -> Maybe ( a, b )
        mapper maybeValue =
            case maybeValue of
                Just ( originalKey, val ) ->
                    -- convoluted way of applying function (Maybe b -> Maybe b) to (Maybe (a, b))
                    val
                        |> Just
                        |> f
                        |> Maybe.map identity
                        |> Maybe.withDefault val
                        |> Tuple.pair originalKey
                        |> Just

                _ ->
                    Nothing
    in
    { opaque
        | dict =
            dict |> Dict.update (key |> keyToString) mapper
    }


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary.
-}
union : OpaqueDict a b -> OpaqueDict a b -> OpaqueDict a b
union one another =
    { one
        | dict = Dict.union one.dict another.dict
    }


{-| Keep a key-value pair when its key appears in the second dictionary. Preference is given to values in the first dictionary.
-}
intersect : OpaqueDict a b -> OpaqueDict a b -> OpaqueDict a b
intersect one another =
    { one
        | dict = Dict.intersect one.dict another.dict
    }


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : OpaqueDict a b -> OpaqueDict a b -> OpaqueDict a b
diff one another =
    { one
        | dict = Dict.diff one.dict another.dict
    }


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.
    You then traverse all the keys from lowest to highest, building up whatever
    you want.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> OpaqueDict k a
    -> OpaqueDict k b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        subLeftStep : String -> ( k, a ) -> result -> result
        subLeftStep _ ( key, val ) subResult =
            leftStep key val subResult

        subBothStep : String -> ( k, a ) -> ( k, b ) -> result -> result
        subBothStep _ ( key, leftVal ) ( _, rightVal ) subResult =
            bothStep key leftVal rightVal subResult

        subRightStep : String -> ( k, b ) -> result -> result
        subRightStep _ ( key, val ) subResult =
            rightStep key val subResult
    in
    Dict.merge subLeftStep subBothStep subRightStep leftDict.dict rightDict.dict initialResult


{-| Get the dictionary values.
-}
values : OpaqueDict key val -> List val
values =
    toList >> List.map Tuple.second


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : OpaqueDict key val -> List ( key, val )
toList { dict } =
    dict
        |> Dict.values


{-| Make the opaque dictionary from a list.
-}
fromList : (key -> String) -> List ( key, val ) -> OpaqueDict key val
fromList keyToString list =
    { keyToString = keyToString
    , dict =
        list
            |> List.map (\( k, v ) -> ( k |> keyToString, ( k, v ) ))
            |> Dict.fromList
    }


{-| Determine the number of key-value pairs in the dictionary.
-}
size : OpaqueDict key val -> Int
size =
    toList >> List.length
