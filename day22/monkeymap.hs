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


chdir 'R' = R
chdir 'L' = L


parseOrders = do
    -- d <- get
    -- n <- number
    -- d <- satisfy (flip elem "RL")
    -- traceShowM (n, d)
    -- return [(n,chdir d)]
    l <- many1 parseNumDir
    eof
    return l

parseNumDir = do
    d <- satisfy (flip elem "RL")
    n <- number
    traceShowM (d, n)
    return (chdir d, n)


completeLine w l = l ++ (replicate m ' ')
  where len = length l
        m = w - len


main = do
    contents <- getContents
    let (inimap,_:ordersline) = span (/= "") $ lines contents
        width = length $ head inimap
        height = length inimap
        grid = listArray ((0, 0), (height-1, width-1)) $ concat $ map (completeLine width) inimap

        ordersl = readP_to_S parseOrders ('R': head ordersline)

    let orders = fst . head $ readP_to_S parseOrders ('R': head ordersline)

    print grid
    print orders

    let initdir = U -- we added a first R to make it more consistent

        initpos = to grid R (0, 0)

    putStrLn $ "Initial position " ++ show initpos

    let (d, (r, c)) = executeOrders grid initdir initpos orders

    print $ (d, r+1, c+1)
    putStrLn $ "Final value: " ++ show (computeFinal d r c) 


computeFinal d r c = dirVal d + 4 * (c+1) + 1000 * (r+1)

dirVal R = 0
dirVal D = 1
dirVal L = 2
dirVal U = 3



executeOrders grid dir p [] = (dir, p)
executeOrders grid dir p ((d, n):ors) =
    trace ("Current position " ++ show (dir, p)) $
    executeOrders grid dir' p' ors
  where p' = go grid dir' p n
        dir' = chd dir d




go grid dir (r, c) 0 = (r, c)
go grid dir p n =
    let p' = to grid dir p
    in
    if grid!p' == '.' then go grid dir p' (n-1)
                      else p


data Direct = R | L | U | D deriving (Eq, Show, Read)

drc D = (1, 0)
drc U = (-1, 0)
drc L = (0, -1)
drc R = (0, 1)

chd R R = D
chd R L = U
chd L R = U
chd L L = D
chd D R = L
chd D L = R
chd U R = R
chd U L = L



to grid dir p = findPos grid p (drc dir)


findPos grid (r, c) (dr, dc) = traceShow (r,c,dr,dc) $
    let r' = (r+dr+h+1) `mod` (h+1)
        c' = (c+dc+w+1) `mod` (w+1)
        (_, (h,w)) = bounds grid
    in
    if grid!(r',c') == ' ' then findPos grid (r', c') (dr, dc)
                           else (r', c')



