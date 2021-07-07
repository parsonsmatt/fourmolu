module ColumnLimitTest where

-- Less than 80 characters
import Data.List (head, isPrefixOf, tail)

-- Over 80 characters, should break this line when column-limit is set to 80.
import Data.Maybe (catMaybe, fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe, maybeToList)

-- For reference, this line had exactly 80 characters -------------------------|

oneVeryLongLine :: [String]
oneVeryLongLine = ["akjsndjklansdsad"] ++ ["jkanskdjnajsndjnasd"] ++ ["jknasdljknasdlnasdn"] ++ ["lajndljnasdlnasds"]

oneVeryLongList :: [String]
oneVeryLongList = ["akjsndjklansdsad", "jkanskdjnajsndjnasd", "jknasdljknasdlnasdn", "lajndljnasdlnasds"]

data NewDataType = NewDataType {field1 :: String, field2 :: String, field3 :: String, field4 :: String}

-- Test if the current line breaking still works normally, i.e. if the user
-- breaks the line, fourmolu breaks and aligns the rest.
data SecondDataType = SecondDataType
    { field1 :: String
    , field2 :: String
    , field3 :: String
    , field4 :: String
    }

data DataTypeWithAVeryLongName = DataTypeWithAVeryLongName String String String String String

-- Long function signatures

-- For reference, this line had exactly 80 characters -------------------------|

longFunction0 :: String -> String -> String -> Maybe Int -> Maybe Int -> Maybe Int -> String -> [String]
longFunction0 veryLongArg1 a b c d e f = ["a list", "of strings", "that will break the", "column limit"]

longFunction1 :: String -> String -> String -> Maybe Int -> Maybe Int -> Maybe Int -> String
longFunction1
    veryLongArg1
    veryLongArg2
    veryLongArg3
    veryLongArg4
    veryLongArgument5
    veryLongArg6 = undefined

longFunction12 veryLongArg1 veryLongArg2 veryLongArg3 veryLongArg4 veryLongArgument5
longFunction12 ::
    String ->
    String ->
    String ->
    Maybe Int ->
    Maybe Int ->
    Maybe Int ->
    String ->
    String
        veryLongArg6
        veryLongArg6 = undefined

longFunction2 :: String -> String -> String -> Maybe Int -> Maybe Int -> Maybe Int -> String
longFunction2 veryLongArg1 veryLongArg2 veryLongArg3 veryLongArg4 a veryLongArg6 =
    let aVeryLongLine = ["list one", "list one", "list one", "list one", "list one", "list one"]
     in undefined

longFunction3 :: String -> String -> String -> Maybe Int -> Maybe Int -> Maybe Int -> String
longFunction3 veryLongArg1 veryLongArg2 veryLongArg3 veryLongArg4 veryLongArgument5 veryLongArg6 = undefined
