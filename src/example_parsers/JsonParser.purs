module JsonParser where

import Prelude hiding (zero)
import Data.Ord
import Data.Int
import Data.Maybe
import Data.String
import Data.Functor
import Data.Tuple
import Data.List hiding (many)
import Data.Array as A
import Control.Alt
import TinyParsec
import Data.Eulalie.Char.Predicates

type PropName = String
data JsonVal
    = JsonInt Int
    | JsonString String
    | JsonBool Boolean
    | JsonObj (List (Tuple String JsonVal))

jsonValParser
    = do
        _ <- pure 1
        jsonIntParser <|> jsonBoolParser <|> jsonStringParser <|> jsonObjParser

propValParser :: Parser (Tuple String JsonVal)
propValParser = do
    prop <- stringLitParser
    _ <- symb ":"
    val <- jsonValParser
    pure (Tuple prop val)

listOfPropValParser :: Parser (List (Tuple String JsonVal))
listOfPropValParser = do
    _ <- pure 1 
    sepBy propValParser (symb ",")

{--jsonObjParser = jsonIntParser--}
--jsonObjParser :: Parser JsonVal
jsonObjParser = do
    _ <- symb "{"
    propValList <- listOfPropValParser
    _ <- symb "}"
    pure (JsonObj propValList)

jsonIntParser :: Parser JsonVal
jsonIntParser = map JsonInt intParser

notDoubleQuote :: Parser Char
notDoubleQuote = sat (_ /= '"')

-- parses string literals, i.e. string in quotes
stringLitParser :: Parser String
stringLitParser = do
    _ <- char '"'
    as <- many notDoubleQuote
    _ <- char '"'
    pure (fromCharArray (A.fromFoldable as))

jsonStringParser :: Parser JsonVal
jsonStringParser = map JsonString stringLitParser   

boolParser :: Parser Boolean
boolParser = (do
                 _ <- string "true"
                 pure true) <|>
                 (do
                 _ <- string "false"
                 pure false)

jsonBoolParser :: Parser JsonVal
jsonBoolParser = map JsonBool boolParser
instance showJsonVal :: Show JsonVal where
    show (JsonInt x) = show x
    show (JsonString s) = s
    show (JsonBool b) = show b
    show (JsonObj o) = show o



