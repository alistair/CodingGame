import System.IO
import Control.Applicative
import Control.Monad
import Data.Map.Strict
import Data.Maybe
import Data.Char
import System.FilePath


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Number of elements which make up the association table.
    input_line <- getLine
    let n = read input_line :: Int
    -- Number Q of file names to be analyzed.
    input_line <- getLine
    let q = read input_line :: Int

    lookup <- fmap fromList $ replicateM n $ do
        input_line <- getLine
        let input = words input_line
        return (toLower <$> input!!0, input!!1)

    replicateM q $ do
        fname <- getLine
        let lfname = toLower <$> fname
        let extension = case takeExtension lfname of
              "" -> ""
              ('.':xs) -> xs
        putStrLn $ fromJust $ Data.Map.Strict.lookup extension lookup <|> Just "UNKNOWN"
    return ()
