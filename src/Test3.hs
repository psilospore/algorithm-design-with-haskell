import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- count well
    
    sentence <- getLine
    let words' = words sentence
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn $ unwords [if length w <= 2 then w else head w : (show $ length w - 2) ++ [last w]| w <- words']
