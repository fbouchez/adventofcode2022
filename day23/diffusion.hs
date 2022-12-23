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


-- numrounds = 3
-- numrounds = 10
numrounds = 10000

showGrid grid = unlines $ map getRow $ [lor..hir]
  where ((lor,loc),(hir,hic)) = bounds grid
        getRow r = map (getPos r) $ [loc..hic]
        getPos r c = grid!(r,c)


type Elf = (Int, Int)
type Grid = Array (Int, Int) Char

data Dir = N | S | W | E deriving (Eq, Show)


initorder = [N, S, W, E]

cycleorder (o:os) = os ++ [o]

lookIn N = [(-1,-1), (-1, 0), (-1, 1)]
lookIn S = [(1,-1), (1, 0), (1, 1)]
lookIn W = [(-1,-1), (0, -1), (1, -1)]
lookIn E = [(-1,1), (0, 1), (1, 1)]

lookAround = diagDirs

addPos (r,c) (dr,dc) = (r+dr, c+dc)

diffDir N = (-1, 0)
diffDir S = (1, 0)
diffDir W = (0, -1)
diffDir E = (0, 1)

moveTo d p = addPos p $ diffDir d



main = do
    print lookAround

    contents <- getContents
    let rows = lines contents
        width = length $ head rows
        height = length rows
        grid = listArray ((1, 1), (height, width)) $ concat rows

        elves = getElves grid

    print elves
    print grid
    putStrLn $ "Initial state:"
    putStrLn $ showGrid grid


    -- let nextg = oneBigger grid elves

    -- let nextg = fullRound grid initorder
    -- putStrLn $ showGrid nextg

    let (final, _) = foldl roundWithRotate (grid, initorder) $ [1..numrounds]

    putStrLn $ "Final state:"
    putStrLn $ showGrid final


    let ((lor,loc), (hir,hic)) = rectangle elves
        numelves = length elves
        elves = getElves final

    print $  rectangle elves
    putStrLn $ "Number of empty spaces: " ++ show ((hic-loc+1)*(hir-lor+1) - numelves)




roundWithRotate (grid, orders) nr =
    trace (shows "After round " $ show nr ++ " order " ++ show orders) $
    trace (showGrid nextg) $
    if nextg == grid then 
                      trace (shows "After round " $ show nr ++ " order " ++ show orders) $
                      trace (showGrid nextg) $
                      error $ "Stop at round " ++ show nr
                     else (nextg, cycleorder orders)
  where nextg = fullRound grid orders



rectangle elves = ((lor,loc), (hir,hic))
  where lor = minimum rows
        hir = maximum rows
        loc = minimum cols
        hic = maximum cols
        (rows, cols) = unzip elves



-- get the coordinates of elves in grid
getElves grid = map fst $ filter ((=='#') .  snd) $ assocs grid

oneBigger grid elves = accumArray collision '.' bnds $ zip elves (repeat '#')
  where
        bnds = ((lor-1,loc-1), (hir+1,hic+1))
        ((lor,loc), (hir,hic)) = rectangle elves

oneEmpty grid = listArray (bounds grid) $ repeat '.'



fullRound grid dirorder =
    let elves = getElves grid
        nextg = oneBigger grid elves
    in
    let propose = firstPart nextg elves dirorder
        gride = oneEmpty nextg
    in
    let gridprop = accumArray collision '.' (bounds nextg) (zip propose $ repeat '#')

    in
    if gridprop == nextg 
      then error "Stopping now"
      else
    -- trace ("Collision grid: \n" ++ showGrid gridprop) $
        secondPart gridprop $ zip elves propose


collision '.' '#' = '#'
collision '#' '#' = 'C' -- collision


firstPart :: Grid -> [Elf] -> [Dir] -> [Elf]
firstPart g e dirs = map (moveIfNeeds dirs) e
  where moveIfNeeds dirs p =
          if all ((=='.') . (g!)) $ map (addPos p) lookAround
            then p
            else tryMove dirs p

        tryMove [] p = p
        tryMove (d:rd) p =
          if all ((=='.') . (g!)) $ map (addPos p) (lookIn d)
            then moveTo d p
            else tryMove rd p



secondPart grid eupdate = accumArray collision '.' (bounds grid) actuallist
  where actuallist = map canmoveto eupdate
        canmoveto (p,p') = if grid!p' == 'C' then (p, '#') else (p', '#')


