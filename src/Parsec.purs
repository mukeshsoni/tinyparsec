module TinyParsec where

import Prelude
import Data.List hiding (uncons, singleton)
import Data.Tuple
import Data.String
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Alt

newtype Parser a = Parser (List Char -> List (Tuple a (List Char)))

parse :: forall a. Parser a -> List Char -> List (Tuple a (List Char)) 
parse (Parser p) s = p s 

item :: Parser Char
item = Parser (\s ->
       case s of
            Nil -> Nil
            (c:cs) -> fromFoldable [Tuple c cs] 
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
    pure a = Parser (\s -> fromFoldable [Tuple a s])

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
char c = sat (\c1 -> c1 == c)

string :: String -> Parser String
string "" = pure ""
string s = 
    let
        headTail = uncons s
    in
        case headTail of
             Nothing -> zero
             Just ht -> do
                 _ <- char ht.head
                 _ <- string ht.tail
                 pure (singleton ht.head <> ht.tail)

many :: Parser a -> Parser (List a)
many p = many1 p <|> pure []

many1 :: Parser a -> Parser (List a)
many1 p = do
    a <- p
    la <- many p
    pure (Cons a la)
