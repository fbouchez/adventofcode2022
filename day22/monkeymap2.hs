import AoC
import Control.Exception
import Data.Char
import Data.List
import Data.List.Split
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
    char '\n'
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



-- cube side, direction, and row, column
data Position = Position Int Direct (Int, Int) deriving(Show)


main = do
    contents <- getContents

    let (dimens:blocs) = splitOn "\n\n" contents

    let [height, width] = map read $ words $ dimens :: [Int]

    print (height, width)

    let (sides, ordersline:[]) = splitAt 6 blocs
        cube = listArray (1,6) $ map (scanSide height width) sides 

        ordersl = readP_to_S parseOrders ('R': ordersline)

    print ordersl

    let orders = fst . head $ ordersl

    print cube
    print orders

    let initpos = Position 1 U (0, 0)

    putStrLn $ "Initial position " ++ show initpos

    -- let (d, (r, c)) = executeOrders grid initdir initpos orders
--
    -- print $ (d, r+1, c+1)
    -- putStrLn $ "Final value: " ++ show (computeFinal d r c) 


scanSide h w s = 
    listArray ((0, 0), (h-1, w-1)) s


computeFinal d r c = dirVal d + 4 * (c+1) + 1000 * (r+1)

dirVal R = 0
dirVal D = 1
dirVal L = 2
dirVal U = 3



executeOrders cube pos [] = pos
executeOrders cube pos ((d, n):ors) =
    trace ("Current position " ++ show (pos)) $
    executeOrders cube pos' ors
  where pos' = go cube pos n



go cube pos 0 = pos
go cube pos n =
    let pos'@(Position cn dir p) = to cube pos
    in
    if cube!cn!p == '.' then go cube pos' (n-1)
                        else pos


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



to cube pos = findPos cube pos (drc dir)


findPos cube (Position cn dir (r, c)) (dr, dc)) = traceShow (cn, r,c,dr,dc) $
    let r' = r + dr
        c' = c + dc
        (_, (h,w)) = bounds $ cube!cn
    in
    if inRange (bounds $ cube!cn) (r',c')
      then (r', c')
      else cubeChange cube cn h w (Position dir (r', c'))


cubeChange cube cn h w p@(Position dir (r, c))
  | r < 0 = gos U h w cn p
  | r > h = gos D h w cn p
  | c < 0 = gos L h w cn p
  | c > w = gos R h w cn p
  | otherwise = error "not outside of side"


go U h w 1 (Position dir (r,c)) = assert dir == U $ (2, Position D (0, w-c))
go p h w 1 (Position dir (r,c)) = assert dir == U $ (2, Position D (0, w-c))
go h w 1 (Position dir (r,c)) = assert dir == U $ (2, Position D (0, w-c))
go U h w 1 (Position dir (r,c)) = assert dir == U $ (2, Position D (0, w-c))


side 1 U = rev 2
side 1 L = tr 3
side 1 R = rev 6
side 1 D = reg 4


side 1 U = rev 2
side 1 L = tr 3
side 1 R = rev 6
side 1 D = reg 4






