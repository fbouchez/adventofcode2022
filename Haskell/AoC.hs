{-# LANGUAGE ScopedTypeVariables #-}

module AoC where

-- import System.IO (isEOF)
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Exception.Base
import Debug.Trace
import Data.Maybe
import Data.List
import Data.Ix
import Data.STRef
import Data.Array
import Data.Array.ST
import Data.Char
import Data.Function
import Data.Tuple.Extra
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Text.Printf


-- Functions to easily split inputs

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

coupleWhen :: (Char -> Bool) -> String -> (String, String)
coupleWhen p s = let [a,b] = wordsWhen p s in (a,b)

couple [a,b] = (a,b)
triple [a,b,c] = (a,b,c)

splitComma :: String -> [String]
splitComma = wordsWhen (==',')

getInts :: IO [Int]
getInts = do getLine >>= return . splitComma >>= return . map read
    -- l <- getLine
    -- let si = splitComma l
    -- return $ map read si

sortFst :: Ord a => [(a,b)] -> [(a,b)]
sortFst = sortBy (compare `on` fst)

genhist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
genhist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

type Coord = (Int, Int)
type DigitMap = Array Coord Int

type XYZCoord = (Int, Int, Int)

getIntMap :: IO DigitMap
getIntMap = do
    contents <- getContents
    let lns = lines contents
        height = length lns
        width  = length . head $ lns
    let imap = listArray ((1,1), (height,width)) $ map digitToInt $ join lns
    return imap


showDigitMap :: DigitMap -> String
showDigitMap imap =
    let ((miny, minx), (maxy, maxx)) = bounds imap
        makeline y = join . map (\x -> show $ imap!(y,x)) $ [minx..maxx]
        rows = map (\y -> makeline y) [miny..maxy]
    in unlines rows


crossDirs = [(1,0), (-1,0), (0,1), (0,-1)]
aroundDirs = (,) <$> [-1,0,1] <*> [-1,0,1]
diagDirs = filter (/=(0,0)) $ aroundDirs

neighFuns :: [Coord] -> [Coord -> Coord]
neighFuns dirs = do map makefun dirs
  where makefun (dr,dc) = \(r,c) -> (r+dr,c+dc)

allNeighbours (r,c) = map ($ (r,c)) $ neighFuns crossDirs

allArround (r,c) = map ($ (r,c)) $ neighFuns aroundDirs

neighb :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
neighb arr (r,c) = filter (inRange (bounds arr)) $ allNeighbours (r,c)

neighbounds bnds (r,c) = filter (inRange bnds) $ allNeighbours (r,c)


convertBin :: [Int] -> Int
convertBin = aux 0
  where aux acc [] = acc
        aux acc (v:rp) = aux (2*acc+v) rp




hist :: (Ix a, Num b) => [a] -> Array a b
hist is = genhist (low,high) is
  where low = minimum is
        high = maximum is

lookup' :: (Eq a) => a -> [(a,b)] -> b
lookup' e l = fromJust $ lookup e l


number :: ReadP Int
number = read <$> numberStr

numberStr :: ReadP String
numberStr = do
    neg <- option ' ' (satisfy (=='-'))
    num <- many1 (satisfy isDigit)
    return (neg:num)


genRange :: String -> ReadP (Int, Int)
genRange str = do
    lo <- number
    string str
    hi <- number
    return (lo, hi)

dotRange :: ReadP (Int, Int)
dotRange = genRange ".."

dashRange :: ReadP (Int, Int)
dashRange = genRange "-"
