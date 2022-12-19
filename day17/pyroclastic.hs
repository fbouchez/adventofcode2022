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
-- heightWindow = 50
heightWindow = 200
gameWidth = 7


-- numTotalShapes = 2022              -- Part I
-- numTotalShapes = 10000              -- some test
numTotalShapes = 1000000000000     -- Part II


patternShape = lshap -- for input-small
-- patternShape = hbar -- for my input

initPattCheckocc = 5 -- number of occurences of pattern to check


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


lenShapes = 5
shapes = listArray (1,lenShapes) [hbar, cross, lshap, vbar, squar]

instance Show TShape where
    show (TShape ((1, 4), _)) = "|"
    show (TShape ((4, 1), _)) = "-"
    show (TShape ((2, 2), _)) = "◻"
    show (TShape ((3, 3), (0,1):_)) = "+"
    show (TShape ((3, 3), _)) = "⅃"


data Pattern = Pattern {
             yposition :: Int,   -- height at which pattern occurs
             pdiridx :: Int,      -- Index in the direction array
             shape :: TShape,    -- Tetris shape involved
             nshape :: Int,     -- number of remaining shapes when pattern occurred
             patt :: [Int]      -- Pattern formed by the x-y coordinates at top of board
    }


instance Show Pattern where
    show (Pattern {
                  yposition = y,
                  pdiridx = pdx,
                  shape = sh,
                  nshape = nsh,
                  patt = p
                  }) = show sh ++ " at " ++ show y ++ " dx " ++ show pdx ++ " top: " ++ show p
                  ++ " nrock: " ++ show nsh


data GamePattern =
    Searching [Pattern]
    | Found {
            fpatt :: Pattern,
            nocc :: Int,
            ydiff :: Int,
            nShapDiff :: Int
            }
    | Applied
                                  deriving (Show)



--      currentHeight, shiftHeight, [list of position where shape is at 1]
data TGame = Game {
           gtop :: Int,
           gbot :: Int,
           gNumShapes :: Int, -- number of shapes to place
           shapeIdx :: Int,   -- next shape to use
           dirIdx :: Int,     -- next direction to use
           dirArray :: Array (Int) Direction, -- the full array of direction to cycle
           gpattern :: GamePattern,
           board :: Array (Int, Int) Bool
        }


instance Show TGame where
    show g =
      "Height: " ++ show (gtop g) ++ " shifted by " ++ show (gbot g) ++
      " rocks to go: " ++ show (gNumShapes g) ++
      "\n" ++ mapToStr (gtop g) (board g)

mapToStr top arr = unlines maplines
  where ((lox, loy), (hix, hiy)) = bounds arr
        maplines = foldl acculine [] $ [loy .. top]
        acculine lns y = mapline y : lns
        mapline y = foldl (getval y) "" $ reverse [lox .. hix]
        getval y acc x = booltochr (arr!(x,y)) : acc
        booltochr True = '#'
        booltochr False = '.'




main = do
    contents <- getContents
    let dirlst = map readDir $ filter (/= '\n') contents :: [Direction]
        dirarr = listArray (1, length dirlst) dirlst
        initgame = Game {
                        gtop = 0,
                        gbot = 0,
                        gNumShapes = numTotalShapes,
                        dirIdx = 1,
                        shapeIdx = 1,
                        dirArray = dirarr,
                        gpattern = Searching [],
                        board = listArray ((1, 1), (gameWidth, heightWindow)) (repeat False)
                        }

        finalgame = playgame initgame

    -- print dirlst
    -- print initgame
    print finalgame
    putStrLn $ "Final height: " ++ show (gtop finalgame) ++ " shifted by " ++ show (gbot finalgame)
    putStrLn $ "Final pattern: " ++ show (gpattern finalgame)


playgame game@(Game {gNumShapes = 0}) = game
playgame game@(Game {gNumShapes = nremain}) =
    case playround game of
      Left g -> error $ "not enough height : " ++ show nremain ++ " shapes still to place"
      Right next -> 
        let shapidx' = (shapeIdx game) `mod` lenShapes + 1
        in playgame next



-- playround game@(Game ((gameheight, gamesh, prevSeen), arr)) numDirs curDir dirs shap = -- trace ("Adding piece " ++ show shap) $
playround game = -- trace ("Adding piece " ++ show shap) $
    let initx = 3
        inity = gtop game + 3 + h
        shap@(TShape ((w, h), pos)) = shapes!(shapeIdx game)
        arr = board game
        ((_, _), (_, maxgameheight)) = bounds arr
        darr = dirArray game
        (_, numDirs) = bounds darr
    in
    -- trace ("Init position " ++ show (initx, inity)) $

    let continue diridx x y = --trace ("Dir idx " ++ show curDir ++ " max " ++ show numDirs) $
          let dir = darr!diridx -- current direction
              diridx' = (diridx `mod` numDirs) + 1 -- compute next direction index
          in
          let trymove Dirleft x y = --trace ("Try <") $
                if x > 1 && not (collision arr shap (x-1) y) then x-1 else x

              trymove Dirright x y = --trace ("Try >" ++ show (x+w)) $
                if x+w-1 < gameWidth && not (collision arr shap (x+1) y) then x+1 else x

              trydown x y = --trace ("Try down") $
                if y > 1 && not (collision arr shap x (y-1)) then y-1 else y

          in
          let x' = trymove dir x y
          in
          -- trace ("Now x' is " ++ show x') $
          let y' = trydown x' y
          in
          -- trace ("Now y' is " ++ show y') $
          if y' == y then lockShape game diridx' shap x' y'
                     else continue diridx' x' y'
    in
    if inity > maxgameheight then Left game
                             else Right $ continue (dirIdx game) initx inity


lockShape game diridx s@(TShape ((w,h), pos)) x y = -- trace ("Locking " ++ show s ++ " at position " ++ show (x, y) ++ show prevSeen) $

    let arr = board game
        arr' = arr // poslist
        poslist = zip (shiftPos pos x y) $ repeat True
        patt = gpattern game
        shapidx' = (shapeIdx game) `mod` lenShapes + 1
        rockremain = gNumShapes game
    in


    let game' = game {
                 gtop = max (gtop game) y,
                 dirIdx = diridx,
                 shapeIdx = shapidx',
                 gNumShapes = rockremain-1,
                 board = arr'
    }
    in

    -- checking or updating pattern list
    let patt' =
          if x /= 1
            then patt
            else case patt of
                  Searching pattlst -> updatePatterns game' pattlst s y diridx
                  Found {} -> checkPattern game' patt s y diridx
                  Applied -> patt
    in

    let game'' = case patt' of
                   Found {nocc = 0} -> fastForwardInTime game' patt'
                   _ -> game' { gpattern = patt' }
    -- traceShow patt' $

    in
    -- trace ("locked " ++ show s ++ "\n" ++ show game'') $
    if x == 1 then compressGame game''
              else game''


updatePatterns game pattlst sh y diridx = 
    let top = topPattern game y
    in
    trace ("Searching pattern " ++ show sh ++ " " ++ show top ++ " in " ++ show pattlst) $
      case pattSearch sh y diridx top pattlst of
        Nothing -> Searching $ Pattern {
            yposition = y,
            pdiridx = diridx,
            shape = sh,
            nshape = gNumShapes game,
            patt = top
          } : pattlst
        Just p -> 
          let ydiff = y - yposition p
              nshapd = nshape p - gNumShapes game
          in
          trace ("Found pattern " ++ show p ++ " with ydiff " ++ show ydiff
                ++ " and nrockdiff " ++ show nshapd
                ++ "\n" ++ show game
                ) $
          Found {
                fpatt = p { yposition = y, nshape = gNumShapes game },
                nocc = initPattCheckocc,
                ydiff = ydiff,
                nShapDiff = nshape p - gNumShapes game
                }



pattSearch sh y diridx top [] = Nothing
pattSearch sh y diridx top (p:pr) =
    let Pattern {
                yposition = py,
                pdiridx = pdx,
                shape = psh,
                patt = ptop
                } = p
    in
      if pdx == diridx &&
         psh == sh &&
         ptop == top
         then Just p
         else pattSearch sh y diridx top pr


checkPattern game fp sh y diridx =
    let top = topPattern game y
        nsh = gNumShapes game
        Found {
            fpatt = p,
            nocc = nocc,
            ydiff = ydiff,
            nShapDiff = nshd
            } = fp
        Pattern {
                yposition = py,
                pdiridx = pdx,
                shape = psh,
                nshape = pnsh,
                patt = ptop
                } = p
    in
      if pdx == diridx &&
         psh == sh &&
         ptop == top
         then if ydiff /= (y - py) || nshd /= (pnsh - nsh)
                then error $ "Ydiff does not match pattern: " ++ show ydiff ++ " vs " ++
                       show y ++ "-" ++ show py ++ " = " ++ show (y - py) ++
                       "rock diff: " ++ show nshd ++ " vs " ++ show (pnsh - nsh) ++
                       " prev rock: " ++ show pnsh ++ " vs current " ++ show nsh
                else trace ("Pattern found again after " ++ show ydiff ++ " check:" ++ show nocc
                           ++ " Remaining rocks: " ++ show nsh ++ " prev rock: " ++ show pnsh ++
                           " rock diff: " ++ show nshd
                           )$ 
                  fp { fpatt = p {yposition = y, nshape = nsh}, nocc = (nocc - 1) }
         else -- trace "Not same pattern" $
           fp


topPattern game y = shiftedgamedrawing
  where
    gameDrawing = getGameDrawing y (gbot game) (board game)
    shiftedgamedrawing = map ((-) y) gameDrawing


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


compressGame game = -- trace "trying to compress" $
    let bot = gbot game
        top = gtop game
        newbot = minimum $ getGameDrawing top bot (board game)
        rebuildAt newbot = arr'
        arr = board game
        narr = listArray ((1, newbot), (gameWidth, newbot+heightWindow)) (repeat False)
        arr' = narr // trues
        trues = filter (aboveshift newbot) $ filter ((==) True . snd) $ assocs arr
        aboveshift s ((_,y),_) = y >= s
    in
    if newbot == bot then game
                     else -- trace ("compressing" ++ show game) $
                        game {
                          gbot = newbot,
                          board = rebuildAt newbot
                        }


fastForwardInTime game patt =
    let Game {
             gtop = top,
             gbot = bot,
             gNumShapes = nsh,
             board = arr
             } = game
        Found {
              ydiff = ydiff,
              nShapDiff = nshdiff
              } = patt
    in
    -- compute number of fills
    -- let fills = 1
        -- rockremain = nsh - fills*nshdiff
    let fills = nsh `div` nshdiff
        rockremain = nsh `mod` nshdiff
    in
    trace ("Filling with " ++ show fills ++ " in-between patterns") $
    let yshift = fills * ydiff
        newtop = top + yshift
        newbot = bot + yshift
    in
    let narr = array ((1, newbot), (gameWidth, newbot+heightWindow)) arrcontentsshifted
        arrcontents = assocs arr
        arrcontentsshifted = map doShift arrcontents
        doShift ((x,y), flag) = ((x,y + yshift), flag)
        -- narr = narri // arrcontentsshifted
        -- narr = narri // [((x, newtop-2), True) | x <- [1..gameWidth]]
    in
    -- traceShow (bounds testa) $
    -- traceShow testa $

    -- traceShow arrcontents $
    -- traceShow arrcontentsshifted $
    -- traceShow (bot, top, bot+heightWindow) $
    -- traceShow (newbot, newtop, newbot+heightWindow) $
    -- trace "Tring to show array bounds" $
    -- traceShow (bounds narri) $
    -- traceShow (narr!(0,0)) $
    let g = game {
           gtop = newtop,
           gbot = newbot,
           gNumShapes = rockremain,
           board = narr,
           gpattern = Applied
           }
    in
      traceShow game $
      traceShow g $
      g
