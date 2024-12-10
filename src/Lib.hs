module Lib
    ( 
    ) where

import Control.Applicative ( (<$>), some)
import Text.Parsec
import Text.Parsec.String (Parser)


-- Парсер для числа
number :: Parser Int
number = read <$> some digit

-- Парсер для бинарной операции
binOperation :: Parser (Int -> Int -> Int)
binOperation = do
    op <- oneOf "+-*/"
    return $ case op of
        '+' -> (+)
        '-' -> (-)
        '*' -> (*)
        '/' -> div
        _   -> error "Unknown operator"

