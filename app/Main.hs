module Main where

import Lib (parseExpression, readByLines)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs [fileName] = readByLines fileName >>= processLines
processArgs _ = TIO.putStrLn  $ T.pack "Error. file isnt exists"

processLines :: [T.Text] -> IO ()
processLines = mapM_ parseExpression
