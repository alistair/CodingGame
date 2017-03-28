import System.IO
import Control.Monad
import Control.Applicative
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    width <- readLn :: IO Int
    height <- readLn :: IO Int

    rows <- replicateM height getLine

    let transposed_rows = transpose rows

    hPutStrLn stderr $ "Input: " ++ show (rows,transposed_rows)


    let index = [0..width-1] >>= \x -> return ((,) x) <*> [0..height - 1]

    hPutStrLn stderr $ "Index: " ++ show index

    forM_ index $ \x -> do
      let xi = fst x
      let yi = snd x

      hPutStrLn stderr $ "Index: " ++ show (xi,yi)

      let row = rows !! yi
      let x1 = row !! xi
      let restx = drop (xi+1) $ rows !! yi
      let resty = drop (yi+1) $ transposed_rows !! xi

      hPutStrLn stderr $ "Rest: " ++ show (restx,resty)

      let findx = elemIndex '0' restx
      let findy = elemIndex '0' resty

      case x1 of
        '0' -> putStrLn $ blah xi yi findx findy
        otherwise -> return ()

      return ()
    -- hPutStrLn stderr "Debug messages..."

    -- Three coordinates: a node, its right neighbor, its bottom neighbor

    putStrLn "0 0 1 0 0 1"
    putStrLn "1 0 -1 -1 -1 -1"
    putStrLn "0 1 -1 -1 -1 -1"
    return ()

blah :: Int -> Int -> Maybe Int -> Maybe Int -> String
blah x y Nothing Nothing = cords_to_string x y ++ " -1 -1 -1 -1"
blah x y (Just x1) Nothing = cords_to_string x y ++ " " ++ cords_to_string (x1+x+1) y ++ " -1 -1"
blah x y Nothing (Just y1) = cords_to_string x y ++ " -1 -1 " ++ cords_to_string x (y1+y+1)
blah x y (Just x1) (Just y1) = cords_to_string x y ++ " " ++ cords_to_string (x1+x+1) y ++ " " ++ cords_to_string x (y1+y+1)

cords_to_string :: Int -> Int -> String
cords_to_string x y = (show x) ++ " " ++ (show y)
