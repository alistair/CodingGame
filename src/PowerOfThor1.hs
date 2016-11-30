import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    loop (getDirection lightx lighty) initialtx initialty

loop :: (Int -> Int -> (String, Int, Int)) -> Int -> Int -> IO ()
loop direction x y =
  do input_line <- getLine
     let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
     let (goIn,newx,newy) = direction x y
     putStrLn goIn
     loop direction newx newy

getDirection :: Int -> Int -> Int -> Int -> (String, Int, Int)
getDirection tx ty x y
    | tx == x && ty < y = ("N",    x,      y-1)
    | tx > x && ty < y  = ("NE",   x+1,    y-1)
    | tx > x && ty == y = ("E",    x+1,    y)
    | tx > x && ty > y  = ("SE",   x+1,    y+1)
    | tx == x && ty > y = ("S",    x,      y+1)
    | tx < x && ty > y  = ("SW",   x-1,    y+1)
    | tx < x && ty == y = ("W",    x-1,    y)
    | tx < x && ty < y  = ("NW",   x-1,    y-1)
