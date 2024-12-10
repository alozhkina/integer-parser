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
            Just (cs, c) -> Just (cs, func c) -- (остаток, c обработанный функцией func), если p возвращает (остаток, resP)

-- Парсит символ, если он соответствует предикату
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser f where
        f input = case T.uncons input of
                Just (c, rest) | predicate c -> Just (rest, c)
                _ -> Nothing

-- Парсит цифру и преобразует её в Int
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit


