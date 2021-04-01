import System.IO
import Control.Monad

import Data.Char (ord, chr)
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn [ charShifted | 
        o <- message,
        let charCode = ord o,
        let charCodeMod = charCode + 2,
        let charCodeShifted = if charCode < 91 then charCodeMod + 65 else charCodeMod + 97,
        let charShifted = chr charCode
        ]
    return ()