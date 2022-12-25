import AoC
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Array
import Data.Tuple.Extra

import Debug.Trace
import qualified Data.Text as T
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP


data Stat = Stat {
          nsteps :: Int,
          grid :: CharMap,
          access :: [(Coord)],
          entryp :: Coord,
          exitp :: Coord
          } deriving (Show)



main = do
    grid <- getCharMap

    let entry = (1, 2)
        ((_,_), (height, width)) = bounds grid
        exit = (height, width-1)

    putStrLn $ showCharMap grid


    let final = searchPath (Stat 0 grid [entry] entry exit)
        back =  searchPath (Stat (nsteps final) grid [exit] exit entry)
        again =  searchPath (Stat (nsteps back) grid [entry] entry exit)

    putStrLn $ "Steps: " ++ show (nsteps final)
    putStrLn $ "Steps: " ++ show (nsteps back)
    putStrLn $ "Steps: " ++ show (nsteps again)



searchPath st = until exitReached oneStep st

exitReached st = (exitp st `elem` access st)

collision '.' '#' = '#'

oneStep st@(Stat nstepinit grid access entryp exitp) =
    trace ("Step: " ++ show nstepinit) $
    traceShow access $
    trace (showCharMap (accumArray collision '.' (bounds grid) (zip access $ repeat '#'))) $
    st { nsteps = nstep ,
       access = nub . concat . map availLocNeighb $ access }
  where
    nstep = nstepinit+1
    ((_,_),(height,width)) = bounds grid
    availLocNeighb p = filter isEmpty $ locn p

    locn p@(r,c)
      | p == entryp = (if r == 1 then (2,2) else (height-1,width-1)):entryp:[]
      | otherwise = neighDir crossLocalDirs p


    isSide (r,c) = r == 1 || c == 1 || r == height || c == width

    isEmpty p@(r,c)
      | p == entryp = True
      | p == exitp  = True
      | otherwise =
        (not (isSide p) &&
        not (any (isBlizzard p) [N,S,E,W]))

    isBlizzard :: Coord -> Cardir -> Bool
    isBlizzard p@(r,c) dir = --trace ("Check pos " ++ show (p', pmany, dir))
      grid!p' == dirSym dir
      where pmany = add2 p $ mul2 nstep $ inDir . opposite $ dir
            p' = wrap pmany

    wrap :: Coord -> Coord
    wrap (r,c)
      -- | traceShow (r,c) $ False = error "debug"
      -- | r < 2 = traceShow r $ (traceShowId (traceShowId(r-2) `mod` traceShowId(height-2)) + 2, c)
      | r < 2 = ((r-2) `mod` (height-2) + 2, c)
      -- | c < 2 = (r, (c-2) `mod` (width-2) + width)
      | c < 2 = (r, (c-2) `mod` (width-2) + 2)
      | r > height-1 = ((r-2) `mod` (height-2) + 2, c)
      | c > width-1 = (r, (c-2) `mod` (width-2) + 2)
      | otherwise = (r,c)









