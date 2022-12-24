import AoC
import Control.Exception
import Data.Char
import Data.List
import Data.List.Extra
-- import Data.List.Split -- conflicts with splitOn
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

opposite N = S
opposite S = N
opposite E = W
opposite W = E

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
    -- traceShowM (d, n)
    return (chdir d, n)


completeLine w l = l ++ (replicate m ' ')
  where len = length l
        m = w - len



-- cube side, direction, and row, column
type Side = Int
data Position = Position Side Cardir Coord deriving(Show)

type CubeSide = Array (Coord) Char

type SideRules = Array Side [(Cardir, (Side, Cardir))]

data Cube = Cube {
          sides :: Array Side CubeSide,
          offsets :: [(Int, Int)], -- offset of side in flattened cube perspective
          cwidth :: Int,
          cheight :: Int,
          rules :: SideRules
} deriving (Eq, Show)


-- Shape of cube
--   1
-- 234
--   56
--
createCubeSmallRules :: SideRules
createCubeSmallRules = 
    -- "regular" links between sides

    [] &
    horizontaLink 2 3 &
    horizontaLink 3 4 &
    horizontaLink 5 6 &

    verticalLink 1 4 &
    verticalLink 4 5 &

    -- links with changes in perspective

    connect 1 W 3 N &
    connect 1 N 2 N &
    connect 1 E 6 E &
    connect 2 W 6 S &
    connect 2 S 5 S &
    connect 3 S 5 W &
    connect 4 E 6 N &

    makeSideRules


-- Shape of cube
--   12
--   3
--  45
--  6
createCubeNormalRules :: SideRules
createCubeNormalRules = 
    -- "regular" links between sides

    [] &
    horizontaLink 1 2 &
    horizontaLink 4 5 &

    verticalLink 1 3 &
    verticalLink 3 5 &
    verticalLink 4 6 &

    -- links with changes in perspective

    connect 1 W 4 W &
    connect 1 N 6 W &
    connect 2 N 6 S &
    connect 2 E 5 E &
    connect 2 S 3 E &
    connect 3 W 4 N &
    connect 5 S 6 E &

    makeSideRules


horizontaLink a b l = traceShow l $
    (a, (E, (b, W))) : (b, (W, (a, E))) : l

verticalLink a b l =
    (a, (S, (b, N))) : (b, (N, (a, S))) : l

connect a d b d' l =
    (a, (d, (b, d'))) : (b, (d', (a, d))) : l

makeSideRules l = traceShow l $
    array (1,6) $ groupSort l


changeSide cube pos@(Position sn dir p) =
    let rul = (rules cube)!sn
        (sn', dir') = fromJust . lookup dir $ rul
        w = cwidth cube
        h = cheight cube

        sidetoside N S (r, c) = assert (r == 1) $ (h, c)
        sidetoside S N (r, c) = assert (r == h) $ (1, c)
        sidetoside E W (r, c) = assert (c == w) $ (r, 1)
        sidetoside W E (r, c) = assert (c == 1) $ (r, w)

        sidetoside N N (r, c) = assert (r == 1) $ (1, w-c+1)
        sidetoside N W (r, c) = assert (r == 1) $ (c, 1)
        sidetoside N E (r, c) = assert (r == 1) $ (h-c+1, w)

        sidetoside E N (r, c) = assert (c == w) $ (1, w-r+1)
        sidetoside E E (r, c) = assert (c == w) $ (h-r+1, w)
        sidetoside E S (r, c) = assert (c == w) $ (h, r)

        sidetoside W N (r, c) = assert (c == 1) $ (1, r)
        sidetoside W W (r, c) = assert (c == 1) $ (h-r+1, 1)
        sidetoside W S (r, c) = assert (c == 1) $ (h, w-r+1)

        sidetoside S E (r, c) = assert (r == h) $ (c, w)
        sidetoside S W (r, c) = assert (r == h) $ (h-c+1, 1)
        sidetoside S S (r, c) = assert (r == h) $ (r, w-c+1)

        sidetoside a b p = error $ "Not implemented: " ++ show (a,b, p)
    in
    let p' = sidetoside dir dir' p
        pos' = Position sn' (opposite dir') p'
    in
    trace ("Changing side " ++ show pos ++ " to " ++ show pos') $
    pos'





main = do

    contents <- getContents

    let (dimens:blocs) = splitOn "\n\n" contents

    let [height, width] = map read $ words $ dimens :: [Int]

    print (height, width)

    let (sideslns, ordersline:[]) = splitAt 6 blocs

        (offsets, sds) = unzip $ map (scanSide height width) sideslns
        sides = listArray (1,6) $ sds

        ordersl = readP_to_S parseOrders ('R': ordersline)
        cube = Cube {
                    sides = sides,
                    offsets = (0,0):offsets,
                    cwidth = width,
                    cheight = height,
                    -- rules = createCubeSmallRules
                    rules = createCubeNormalRules
                    }

    -- print cube
    -- print ordersl

    let orders = fst . head $ ordersl

    -- print cube
    print orders

    let initpos = Position 1 N (1, 1)

    putStrLn $ "Initial position " ++ show initpos

    let finalpos = executeOrders cube initpos orders

    print $ finalpos
    -- putStrLn $ "Final value: " ++ show (computeFinal d r c) 
    putStrLn $ showString "Final password: " $ show $ computeFinal cube finalpos

scanSide h w s = ((roff, coff), listArray ((1, 1), (h, w)) s')
  where (hd:lns) = lines s
        s' = concat lns
        [_, roff, coff] = traceShowId $ map read $ words hd


computeFinal cube (Position sn d p) = dirVal d + 4 * c' + 1000 * r'
  where (r', c') = add2 p sideoffset
        sideoffset = (roff * cheight cube, coff * cwidth cube)
        (roff, coff) = (offsets cube)!!sn

dirVal E = 0
dirVal S = 1
dirVal W = 2
dirVal N = 3


executeOrders cube pos orders = foldl (execOrder cube) pos orders

execOrder cube pos o@(turn, nsteps) =
    go cube (changeDir pos turn) nsteps


changeDir (Position s d p) turn = trace ("Changing direction " ++ show turn) $
    Position s (chd d turn) p

chd E R = S
chd E L = N
chd W R = N
chd W L = S
chd S R = W
chd S L = E
chd N R = E
chd N L = W



go cube pos 0 = pos
go cube pos n = 
    let pos'@(Position sn dir p) = oneStep cube pos
    in
    if (sides cube)!sn!p == '.' then go cube pos' (n-1)
                                else trace "\tObstacle !" $
                                  pos


-- oneStep cube pos = findPos cube pos (inDir dir)

oneStep cube pos@(Position sn dir p) = trace ("One step from " ++ show pos) $
    let diffp = inDir dir
        p' = add2 p diffp
        h = cheight cube
        w = cwidth cube
    in
    if inRange (bounds $ (sides cube)!sn) p'
      then Position sn dir p'
      -- else cubeChange cube (Position sn dir p')
      else changeSide cube pos


-- cubeChange cube@(Cube { cwidth = w, cheight = h }) p@(Position side dir (r, c))
  -- | r < 1 = goside N cube p
  -- | r > h = goside S cube p
  -- | c < 1 = goside W cube p
  -- | c > w = goside E cube p
  -- | otherwise = error "not outside of side"


-- goside _ _ p = error "TODO"





