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
import qualified Text.ParserCombinators.ReadP as T
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

ap3 f (x,y,z) = (f x, f y, f z)

add2 (x,y) (a,b) = (x+a, y+b)
add3 (x,y,z) (a,b,c) = (x+a, y+b, z+c)



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
type CharMap = Array Coord Char

type XYZCoord = (Int, Int, Int)

getCharMap :: IO CharMap
getCharMap = do
    contents <- getContents
    let rows = lines contents
        height = length rows
        width  = length . head $ rows
    let imap = listArray ((1,1), (height,width)) $ concat rows
    return imap



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


showCharMap grid = unlines $ map getRow $ [lor..hir]
  where ((lor,loc),(hir,hic)) = bounds grid
        getRow r = map (getPos r) $ [loc..hic]
        getPos r c = grid!(r,c)


data Turn = L | R deriving (Eq, Show)
data Cardir = N | S | W | E deriving (Eq, Show)


inDir N = (-1, 0)
inDir S = (1, 0)
inDir W = (0, -1)
inDir E = (0, 1)

lookIn N = [(-1,-1), (-1, 0), (-1, 1)]
lookIn S = [(1,-1), (1, 0), (1, 1)]
lookIn W = [(-1,-1), (0, -1), (1, -1)]
lookIn E = [(-1,1), (0, 1), (1, 1)]

moveTo dir p = add2 p $ inDir dir

crossDirs = [(1,0), (-1,0), (0,1), (0,-1)]
localArea = (,) <$> [-1,0,1] <*> [-1,0,1]
aroundDirs = filter (/=(0,0)) $ localArea

neighFuns :: [Coord] -> [Coord -> Coord]
neighFuns dirs = map add2 dirs


neighDir dirs (r,c) = map ($ (r,c)) $ neighFuns dirs

crossNeighbours = neighDir crossDirs
allNeighbours = neighDir aroundDirs

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

parseIdent :: T.ReadP String
parseIdent = do
    c <- T.satisfy isAlpha
    rst <- T.many $ T.satisfy isAlphaNum
    return $ c:rst

isAlnumDot c =
    isAlphaNum c || c == '.' || c == '/'



number :: T.ReadP Int
number = read <$> numberStr

numberStr :: T.ReadP String
numberStr = do
    neg <- T.option ' ' (T.satisfy (=='-'))
    num <- T.many1 (T.satisfy isDigit)
    return (neg:num)


genRange :: String -> T.ReadP (Int, Int)
genRange str = do
    lo <- number
    T.string str
    hi <- number
    return (lo, hi)

dotRange :: T.ReadP (Int, Int)
dotRange = genRange ".."

dashRange :: T.ReadP (Int, Int)
dashRange = genRange "-"
