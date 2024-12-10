module Lib
    ( 
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ( (<$>), some)
import Data.Char (isDigit, digitToInt)

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

instance Functor Parser where
    -- хотим применить func над результатом парсера p
    fmap func (Parser p) = Parser f where
        -- парсер f возвращает:
        f input = case p input of
            Nothing -> Nothing -- Nothing, если парсер p возвращает Nothing
            Just (cs, c) -> Just (cs, func c) -- (остаток, c обработанный функцией func), если p возвращает (остаток, c)

-- Парсит символ, если он соответствует предикату
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser f where
        f input = case T.uncons input of
                Just (cs, c) | predicate cs -> Just (c, cs)
                _ -> Nothing

-- Парсит цифру и преобразует её в Int
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- Парсит последовательность цифр и преобразует её в Int
integer :: Parser Int
integer = Parser f
    where
        f xs =
            let (digits, cs) = T.span isDigit xs
            in if T.null digits
                then Nothing
                else Just (cs, T.foldl' (\acc c -> acc * 10 + digitToInt c) 0 digits)

-- Парсит пробел
space :: Parser Char
space = satisfy (== ' ')

-- Парсит пробелы
spaces :: Parser ()
spaces = Parser f
    where
        f xs =
            let cs = T.dropWhile (== ' ') xs
            in Just (cs, ())


-- Парсер для оператора
operation :: Parser Char
operation = satisfy (`elem` "+-*/")