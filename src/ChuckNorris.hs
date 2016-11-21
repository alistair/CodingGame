{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad
import System.IO
import Text.Printf
import Data.List
import Control.Applicative

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    message <- getLine
    putStrLn $ unwords $ chuck <$> (group $ message >>= encode)
    where
      encode :: Char -> String
      encode = (printf "%07b") . fromEnum

      chuck :: String -> String
      chuck "" = ""
      chuck all@('1':xs) = "0 " ++ (take (length all) $ repeat '0')
      chuck all@('0':xs) = "00 " ++ (take (length all) $ repeat '0')
