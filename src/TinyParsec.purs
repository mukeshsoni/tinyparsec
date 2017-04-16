module TinyParsec where

import Prelude
import Data.List (List(..), (:), singleton, concat, concatMap)
import Data.Tuple (Tuple(..))
import Data.String as S
import Data.Maybe (Maybe(..))
import Control.Alt
import Data.Eulalie.Char.Predicates

newtype Parser a = Parser (String -> List (Tuple a String))

parse :: forall a. Parser a -> String -> List (Tuple a String) 
parse (Parser p) s = p s 

item :: Parser Char
item = Parser (\s ->
       let
            headTail = S.uncons s
       in
           case headTail of
                Nothing -> Nil
                Just parts -> singleton (Tuple parts.head parts.tail) 
                )

instance parserApply :: Apply Parser where
    apply (Parser q) (Parser p) = Parser  
                                    (\s ->
                                    let 
                                        l1 = p s
                                        l2 = q s
                                    in
                                        concatMap (\(Tuple a s1) ->
                                                    map (\(Tuple f s2) -> (Tuple (f a) s2)) l2) l1)

instance parserMap :: Functor Parser where
    map f (Parser p) = Parser (\s -> map (\(Tuple a s') -> (Tuple (f a) s')) (p s))

instance parserApplicative :: Applicative Parser where
    pure a = Parser (\s -> singleton (Tuple a s))

instance parserBind :: Bind Parser where
    bind (Parser p) f = Parser (\s -> 
                        concat (map
                               (\(Tuple a s') -> parse (f a) s') (p s)))

instance parserAlt :: Alt Parser where
    alt p q = 
        Parser (\s ->
            case (parse p s) of
                 Nil -> parse q s
                 pres -> pres
                 )

zero = Parser (\s -> Nil)
-- conditional parsing
-- only parse a character if the given predicate matches
sat :: (Char -> Boolean) -> Parser Char
sat p = do
    c <- item
    if p c then pure c else zero

char :: Char → Parser Char
char c = sat (c == _)

string :: String -> Parser String
string "" = pure ""
string s = 
    let
        headTail = S.uncons s
    in
        case headTail of
             Nothing -> zero
             Just ht -> do
                 _ <- char ht.head
                 _ <- string ht.tail
                 pure (S.singleton ht.head <> ht.tail)

many :: forall a. Parser a -> Parser (List a)
many p = many1 p <|> pure Nil

many1 :: forall a. Parser a -> Parser (List a)
many1 p = do
    a <- p
    la <- many p
    pure (Cons a la)

sepBy :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy p q = sepBy1 p q <|> pure Nil

sepBy1 :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy1 p q = do
    a <- p
    as <- many (q >>= (\_ -> p)) 
    pure (a:as)
    
chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    a <- p
    rest a
      where
          rest a = (do
                   f <- op
                   b <- p
                   rest (f a b)
                   ) <|> pure a

space :: Parser (List Char)
space = many (sat isSpace)

token :: forall a. Parser a -> Parser a
token p = do
    a <- p
    _ <- space
    pure a

symb :: String -> Parser String
symb s = token (string s)

apply1 :: forall a. Parser a -> String -> List (Tuple a String)
apply1 p s = parse 
                (do
                _ <- space
                p) s
