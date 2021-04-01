{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

-- https://www.hackerrank.com/challenges/components-in-graph/problem

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'componentsInGraph' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts 2D_INTEGER_ARRAY gb as parameter.
--
    
componentsInGraph gb = do
    pure ()
    -- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip :: String -> String
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    gbTemp <- readMultipleLinesAsStringArray n
    let gb = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) gbTemp

    let result = componentsInGraph gb

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
