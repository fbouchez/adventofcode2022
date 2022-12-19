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


maximumHeight = 3300
heightWindow = 200
gameWidth = 7


-- numShapes = 2022              -- Part I
numShapes = 10000              -- some test
-- numShapes = 1000000000000     -- Part II


patternShape = lshap -- for input-small
-- patternShape = hbar -- for my input



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


data TShape = TShape ((Int, Int), [(Int, Int)]) deriving (Eq)


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


--      currentHeight, shiftHeight, [list of position where shape is at 1]
data TGame = Game ((Int, Int, [Int]), Array (Int, Int) Bool)

instance Show TGame where
    show (Game ((height, hshift, pr), arr)) =
      "Height: " ++ show height ++ " shifted by " ++ show hshift ++
      "\n" ++ mapToStr arr

mapToStr arr = unlines maplines
  where ((lox, loy), (hix, hiy)) = bounds arr
        maplines = foldl acculine [] $ [loy .. hiy]
        acculine lns y = mapline y : lns
        mapline y = foldl (getval y) "" $ reverse [lox .. hix]
        getval y acc x = booltochr (arr!(x,y)) : acc
        booltochr True = '#'
        booltochr False = '.'




main = do
    contents <- getContents
    let dirlst = map readDir $ filter (/= '\n') contents :: [Direction]
        initgame = Game ((0, 0, []), listArray ((1, 1), (gameWidth, heightWindow)) (repeat False)) :: TGame

        finalgame = playgame initgame numShapes (length dirlst) 1 (cycle dirlst) (cycle shapes)

    -- print dirlst
    -- print initgame
    print finalgame
    let Game ((h, sh, _), _) = finalgame

    putStrLn $ "Final height: " ++ show h ++ " shifted by " ++ show sh








playgame game 0 _ _ _ _ = game
playgame game@(Game (_, arr)) nremain numDirs curDir dirs (shap:shaplst) =
    case playround game numDirs curDir dirs shap of
      Left g -> error $ "not enough height : " ++ show nremain ++ " shapes still to place"
      Right (curDir', dirs', next) -> playgame next (nremain-1) numDirs curDir' dirs' shaplst



playround game@(Game ((gameheight, gamesh, prevSeen), arr)) numDirs curDir dirs shap = -- trace ("Adding piece " ++ show shap) $
    let initx = 3
        inity = gameheight + 3 + h
        TShape ((w, h), pos) = shap
        ((_, _), (gamewidth, maxgameheight)) = bounds arr
    in
    -- trace ("Init position " ++ show (initx, inity)) $

    let continue _ [] x y = error "No direction left!"
        continue curDir (d:dirs') x y = --trace ("Dir idx " ++ show curDir ++ " max " ++ show numDirs) $
          let curDir' = (curDir `mod` numDirs) + 1
          in
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
          if y' == y then (curDir', dirs', lockShape game curDir' shap x' y')
                     else continue curDir' dirs' x' y'
    in
    if inity > maxgameheight then Left game
                             else Right $ continue curDir dirs initx inity


lockShape (Game ((gameheight, sh, prevSeen), arr)) curDir s@(TShape ((w,h), pos)) x y = trace ("Locking " ++ show s ++ " at position " ++ show (x, y) ++ show prevSeen) $

    let arr' = arr // poslist
        poslist = zip (shiftPos pos x y) $ repeat True
    in

    let prevSeen' = if (x == 1) && s == patternShape
          then 
            let gameDrawing = getGameDrawing y sh arr'
                shgamedrawing = map ((-) y) gameDrawing
            in
            trace ("Some shape " ++ show s ++ " at " ++ show curDir ++ " <> " ++
                      show shgamedrawing
                     ) $
            if curDir `elem` prevSeen then trace ("FOUND SAME POSITION " ++ show curDir) $ prevSeen
                                      else curDir:prevSeen
          else prevSeen
    in

    let g = Game ((max gameheight y, sh, prevSeen'), arr')
    in

      -- traceShowId g
    if x == 1 then compressGame g y
              else g





collision arr (TShape ((w,h), pos)) x y = foldl checkposition False $ shiftPos pos x y
  where checkposition flag (x, y) = flag || arr!(x,y)


shiftPos pos x y = map (shift x y) pos
  where shift x y (a, b) = (x+a, y-b)



getGameDrawing y low arr = map searchMinTrue [1..gameWidth]
  where searchMinTrue x = searchMinTrue' x y
        searchMinTrue' x y =
          if y == low then y -- trace ("Cannot compress") $ y
                      else if arr!(x,y) then y
                                        else searchMinTrue' x (y-1)


compressGame game@(Game ((h, sh, pr), arr)) y = -- trace ("Compressing game at " ++ show y) $
    rebuildAt $ minimum $ getGameDrawing y sh arr
  where rebuildAt newsh = 
          if newsh == sh then game
                         -- else trace ("Can compress at " ++ show newsh ++ "\n" ++ show game) $
                         else -- trace ("Can compress at " ++ show newsh) $
                          Game ((h, newsh, pr), arr')
          where
            narr = listArray ((1, newsh), (gameWidth, newsh+heightWindow)) (repeat False)
            arr' = narr // trues
            trues = filter (aboveshift newsh) $ filter ((==) True . snd) $ assocs arr
            aboveshift s ((_,y),_) = y >= s




