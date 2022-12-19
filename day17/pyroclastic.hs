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

data Direction = Dirleft | Dirright deriving (Show)

-- instance Read Direction where
    -- readsPrec _ c =
      -- let (s, r) = get (/= ' ') c
      -- in case s of 
                   -- '<' -> Dirleft
                   -- '>' -> Dirright
                   -- _   -> error "Unknown direction"

readDir '<' = Dirleft
readDir '>' = Dirright


data TShape = TShape ((Int, Int), [(Int, Int)])


hbar = TShape ((4, 1), [(0,0), (1, 0), (2, 0), (3, 0)])
vbar = TShape ((1, 4), [(0,0), (0, 1), (0, 2), (0, 3)])
cross = TShape ((3, 3), [(0,1), (1, 0), (1, 1), (1, 2), (2, 1)])
lshap = TShape ((3, 3), [(0,2), (1, 2), (2, 0), (2, 1), (2, 2)])
squar = TShape ((2, 2), [(0,0), (0, 1), (1, 0), (1, 1)])


shapes = [hbar, cross, lshap, vbar, squar] :: [TShape]

instance Show TShape where
    show (TShape ((1, 4), _)) = "|"
    show (TShape ((4, 1), _)) = "-"
    show (TShape ((2, 2), _)) = "◻"
    show (TShape ((3, 3), (0,1):_)) = "+"
    show (TShape ((3, 3), _)) = "⅃"


data TGame = Game (Int, Array (Int, Int) Bool)

instance Show TGame where
    show (Game (height, arr)) =
      "Height: " ++ show height ++
      "\n" ++ mapToStr arr

mapToStr arr = unlines maplines
  where ((lox, loy), (hix, hiy)) = bounds arr
        maplines = foldl acculine [] $ [loy .. hiy]
        acculine lns y = mapline y : lns
        mapline y = foldl (getval y) "" $ reverse [lox .. hix]
        getval y acc x = booltochr (arr!(x,y)) : acc
        booltochr True = '#'
        booltochr False = '.'



maximumHeight = 3300
numShapes = 2022


main = do
    contents <- getContents
    let dirlst = map readDir $ filter (/= '\n') contents :: [Direction]
        initgame = Game (0, listArray ((1, 1), (7, maximumHeight)) (repeat False)) :: TGame

        finalgame = playgame initgame numShapes (cycle dirlst) (cycle shapes)

    print dirlst
    print initgame
    print finalgame
    let Game (h, _) = finalgame

    putStrLn $ "Final height: " ++ show h








playgame game 0 _ _ = game
playgame game@(Game (height, arr)) nremain dirs (shap:shaplst) =
    case playround game dirs shap of
      Left g -> error $ "not enough height : " ++ show nremain ++ " shapes still to place"
      Right (dirs', next@(Game (h', arr'))) -> playgame next (nremain-1) dirs' shaplst



playround game@(Game (gameheight, arr)) dirs shap = -- trace ("Adding piece " ++ show shap) $
    let initx = 3
        inity = gameheight + 3 + h
        TShape ((w, h), pos) = shap
        ((_, _), (gamewidth, maxgameheight)) = bounds arr
    in
    -- trace ("Init position " ++ show (initx, inity)) $

    let continue [] x y = error "No direction left!"
        continue (d:dirs') x y =
          let trymove Dirleft x y = -- trace ("Try <") $
                if x > 1 && not (collision arr shap (x-1) y) then x-1 else x

              trymove Dirright x y = -- trace ("Try >" ++ show (x+w)) $
                if x+w-1 < gamewidth && not (collision arr shap (x+1) y) then x+1 else x

              trydown x y = -- trace ("Try down") $
                if y > 1 && not (collision arr shap x (y-1)) then y-1 else y

          in
          let x' = trymove d x y
          in
          -- trace ("Now x' is " ++ show x') $
          let y' = trydown x' y
          in
          -- trace ("Now y' is " ++ show y') $
          if y' == y then (dirs', lockShape game shap x' y')
                     else continue dirs' x' y'
    in
    if inity > maxgameheight then Left game
                             else Right $ continue dirs initx inity


lockShape (Game (gameheight, arr)) s@(TShape ((w,h), pos)) x y = trace ("Locking " ++ show s ++ " at position " ++ show (x, y)) $
    let g = Game (max gameheight y, arr // poslist)
    in
      -- traceShowId g
      g
    where poslist = zip (shiftPos pos x y) $ repeat True




collision arr (TShape ((w,h), pos)) x y = foldl checkposition False $ shiftPos pos x y
  where checkposition flag (x, y) = flag || arr!(x,y)


shiftPos pos x y = map (shift x y) pos
  where shift x y (a, b) = (x+a, y-b)
