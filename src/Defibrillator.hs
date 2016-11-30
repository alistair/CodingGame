{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable
import Prelude hiding (takeWhile)
import System.IO

data Defibrillator = Defibrillator
    { ident :: Int
    , name :: T.Text
    , address :: T.Text
    , phone_number :: T.Text
    , longitude :: Double
    , latitude :: Double
    } deriving (Show,Eq)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input_line <- TIO.getLine
    let lon = read $ T.unpack $ T.replace "," "." input_line :: Double
    input_line <- TIO.getLine
    let lat = read $ T.unpack $ T.replace "," "." input_line :: Double
    input_line <- getLine
    let n = read input_line :: Int
    --1;Maison de la Prevention Sante;6 rue Maguelone 340000 Montpellier;04 67 02 21 60;3,87952263361082;43,6071285339217
    input <- replicateM n $ TIO.getLine
    let (Just results) =
            traverse
                (maybeResult .
                 flip feed "" . parse parseLine' . T.replace "," ".")
                input
    TIO.putStrLn $ name $ head $ sortBy (sortDef lon lat) results

sortDef :: Double -> Double -> Defibrillator -> Defibrillator -> Ordering
sortDef lon lat x y = compare distancex distancey
  where
    distancex = distance lon lat (longitude x) (latitude x)
    distancey = distance lon lat (longitude y) (latitude y)

distance
    :: Floating t
    => t -> t -> t -> t -> t
distance longA latA longB latB =
    let x = (longB - longA) * (cos $ (latA + latB) / 2)
        y = latB - latA
    in sqrt (x * x + y * y) * 6371

isDelimiter
    :: Char -> Bool
isDelimiter = (== ';')

parseLine' :: Parser Defibrillator
parseLine' =
    Defibrillator
      <$> decimal <* char ';'
      <*> takeString
      <*> takeString
      <*> takeString
      <*> double <* char ';'
      <*> double
  where
    takeString = takeTill isDelimiter <* char ';'

parseLine :: Parser Defibrillator
parseLine = do
    ident <- decimal
    char ';'
    name <- takeTill isDelimiter
    char ';'
    address <- takeTill isDelimiter
    char ';'
    phone_number <- takeTill isDelimiter
    char ';'
    lat <- double
    char ';'
    long <- double
    return $ Defibrillator ident name address phone_number lat long
  where
    isDelimiter = (== ';')
