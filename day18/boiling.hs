import AoC
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Array
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP


--- Tiny input
-- maxx = 3
-- maxy = 2
-- maxz = 2

--- Small input
-- maxx = 4
-- maxy = 4
-- maxz = 7

--- Regular input
maxx = 20
maxy = 20
maxz = 20



ibounds = ((0, 0, 0), (maxx, maxy, maxz)) :: (XYZCoord, XYZCoord)
safebounds = (trap (flip (-) 1) (fst ibounds), trap (+1) (snd ibounds)) :: (XYZCoord, XYZCoord)

space = listArray safebounds $ repeat False

trap f (x,y,z) = (f x, f y, f z)

main = do
    contents <- getContents
    let cubesl = lines contents
        cubes = map (triple . map read . splitComma) cubesl :: [XYZCoord]
        update = zip cubes $ repeat True
        lava = space // update 

    -- print cubes
    -- print lava

    let surf = scanLava lava
    print surf
    print around
    let out = outside lava
    print out
    let outsurf = scanOutLava lava out
    putStrLn $ "Outside surface: " ++ show outsurf



-- around = filter (\ (a,b,c) -> a+b+c == 1) [(dx, dy, dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1]]
around = concat [[(dx, 0, 0), (0, dx, 0), (0, 0, dx)] | dx <- [-1,1]]

-- allaround = filter (/= (0,0,0)) [(dx, dy, dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1]]


scanLava lava = foldl countSurf 0 $ indices lava
  where countSurf acc p = if lava!p then foldl countNeighb acc around else acc
          where countNeighb acc' dp = let p' = add3 p dp in
                  -- trace ("Accessing " ++ show p') $
                  acc' + if lava!p' then 0 else 1


scanOutLava lava out = foldl countSurf 0 $ range ibounds
  where countSurf acc p = if lava!p then foldl countNeighb acc around else acc
          where countNeighb acc' dp = let p' = add3 p dp in
                  -- trace ("Accessing " ++ show p') $
                  acc' + if out!p' then 1 else 0




-- outside of lava must be either on the edge, or touching another outside 
-- lava.
-- Works thanks to lazyness, by referring to self array => Actually never 
-- ends because of infinite loops of pockets of air inside the lava :-(
outsideinfiloop lava = outarray 
  where outarray = listArray safebounds $ map outAndHasNeighb $ range safebounds
        outAndHasNeighb p@(x,y,z) = -- trace ("Generating for " ++ show p) $
          if not (inRange ibounds p)
            then True -- border
            else if lava!p
                   then False -- lava is not in outside
                   else hasOutNeighb p -- check if one neighbour is out

          where hasOutNeighb p = foldl checkNeighb False around
                checkNeighb flag (dx,dy,dz) = flag || outarray!(x+dx,y+dy,z+dz)


-- outside of lava must be either on the edge, or touching another outside 
-- lava.
-- Works thanks to lazyness, by referring to self array
outside lava = fixit iterarray initarray
  where initarray = listArray safebounds $ map (not . inRange ibounds) $ range safebounds

        fixit f a = let a' = f a in if a == a' then a else fixit f a'

        iterarray prev = listArray safebounds $ map outAndHasNeighb $ range safebounds
          where
            outAndHasNeighb p@(x,y,z) = -- trace ("Generating for " ++ show p) $
              if not (inRange ibounds p)
                then True -- border
                else if lava!p
                      then False -- lava is not in outside
                      else hasOutNeighb p -- check if one neighbour is out

              where hasOutNeighb p = foldl checkNeighb False around
                    checkNeighb flag pd = flag || prev!(add3 p pd)



countBlocks space = foldl countTrue 0 $ indices space
  where countTrue acc p = acc + if space!p then 1 else 0

