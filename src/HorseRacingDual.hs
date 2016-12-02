import System.IO
import Control.Monad
import Data.List
import Control.Applicative

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line

    horses <- replicateM n $ do
        input_line <- getLine
        return $ read input_line :: IO Int

    let sorted = sort horses
    let pairs = zip sorted $ tail sorted
    let result = foldr (\(x,y) r -> min (abs $ x-y) r) maxBound pairs

    putStrLn $ show $ result
    return ()

-- someone elses solution for comparison
main1 = do
    n <- readLn
    increasingPowers <- sort <$> replicateM n readLn
    let d = minimum $ map (uncurry (-)) $ zip (tail increasingPowers) increasingPowers
    putStrLn (show d)
