import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List

chars = ['a'..'z'] ++ ['?']

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE


    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    t <- getLine

    let indexes = getCharIndex <$> t

    asci <- replicateM h getLine

    mapM (\x -> putStrLn ((getASCI x l) =<< indexes )) asci

    -- hPutStrLn stderr "Debug messages..."

    return ()

getCharIndex :: Char -> Maybe Int
getCharIndex c = elemIndex (toLower c) chars

getASCI :: String -> Int -> Maybe Int -> String
getASCI s l Nothing = drop (26*l) s
getASCI s l (Just i) = take l $ drop (i*l) s
