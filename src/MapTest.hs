import System.IO
import Control.Monad
import Data.List.NonEmpty (sortWith)

type Cords = (Int, Int)
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let s = read input_line :: Int
    
    lines <- replicateM s $ do getLine
    let linesWithRow = zip lines [0..(length lines -1)]
    let charsWithCoords = [(char, (row, col)) | (line, row) <- linesWithRow, (char, col) <- zip line [0..length line - 1], char /= '.' || char /= '*']
    let sorted = sortWith charsWithCoords fst
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn "answers"
    return ()