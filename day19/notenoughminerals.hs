import AoC
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


-- inittime = 10 -- for testing purposes
-- inittime = 19 -- for testing purposes
-- inittime = 21 -- for testing purposes
-- inittime = 24 -- part 1
inittime = 32 -- part 2

parseBluePrints = do
    -- bp <- parseBluePrint
    -- skipSpaces
    -- traceShowM bp
    -- bp' <- parseBluePrint
    -- traceShowM bp'
    -- skipSpaces
    bps <- sepBy1 parseBluePrint skipSpaces
    skipSpaces
    eof
    return bps
    -- return [bp, bp']


data BluePrint = BluePrint {
        ident :: Int,  -- Blueprint number
        rorecost :: Int,   -- cost in ore
        rclaycost :: Int,  -- cost in ore
        robscost :: (Int, Int), -- cost in ore / clay
        rgeocost :: (Int, Int),  -- cost in ore / obs
        maxorerate :: Int,   -- max usefull production rate for ore
        maxclayrate :: Int,
        maxobsrate :: Int
      }
    deriving(Show)


parseBluePrint = do
    string "Blueprint "
    id <- number
    char ':'
    skipSpaces
    string "Each ore robot costs "
    orecost <- number
    string " ore."
    skipSpaces
    string "Each clay robot costs "
    claycost <- number
    string " ore."
    skipSpaces
    string "Each obsidian robot costs "
    obsidcostOre <- number
    string " ore and "
    obsidcostClay <- number
    string " clay."
    skipSpaces
    string "Each geode robot costs "
    geodecostOre <- number
    string " ore and "
    geodecosteObs <- number
    string " obsidian."
    return $ analyseBP id orecost claycost (obsidcostOre, obsidcostClay) (geodecostOre, geodecosteObs)


analyseBP id orecost claycost (obsidcostOre, obsidcostClay) (geodecostOre, geodecosteObs) =
    BluePrint id orecost claycost (obsidcostOre, obsidcostClay) (geodecostOre, geodecosteObs) maxorerate maxclayrate maxobsrate
    where maxorerate = maximum [orecost, claycost, obsidcostOre, geodecostOre]
          -- maxclayrate = (obsidcostClay + 2) `div` 2      -- heuristics to speedup only for part 1
          maxclayrate = obsidcostClay
          maxobsrate = geodecosteObs



data SimuState = SimuState {
        bluep :: BluePrint,
        best :: Array Int [Status] -- keep the best status found at each point
} deriving (Show)



-- add new state and remove all states that are lower
updateSimuState sim@(SimuState bp best) st@(Status time r m) =
    -- trace ("Updating list from\n" ++ show (best!time) ++ "\n" ++ show stupdate) $
    sim { best = best // [(time, stupdate )] }
    where stupdate  = st:(filter (not . flip lowerStatus st) $ best!time)


main = do
    contents <- getContents
    let blueprintsraw = readP_to_S parseBluePrints contents
        blueprints = fst . head $ blueprintsraw

    -- print blueprintsraw
    print blueprints

    -- let bp = blueprints!!0
    let initsimstate bp = SimuState {
          bluep = bp,
          best = listArray (0, inittime) $ repeat []
        }


    -- let bp = blueprints!!0
    -- let testpath = [Rore, Rore, Rclay, Rclay, Rclay, Rclay, Rclay, Rclay, Robs, Robs, Robs, Rgeo, Rgeo]
        -- sol = simulate bp initrobstate initminstate inittime testpath
    -- print sol


    let dobrute bp = bruteForceBP (initsimstate bp) [] bp initrobstate initminstate inittime
    let allbest = 
          if inittime == 32 then map dobrute $ take 3 blueprints
                            else map dobrute blueprints

    mapM_ showthebest allbest

    let result = 
          if inittime == 32 then computeResult32 allbest
                            else computeResult allbest
    putStrLn $ "Résultat : " ++ show result

    -- let (numgeodes, path, sims) = bruteForceBP initsimstate [] bp initrobstate initminstate inittime
    -- putStrLn $ "Nombre de géodes ouvertes: " ++ show numgeodes
    -- putStrLn $ "Simulating path: " ++ show (reverse path)

    -- let testpath = [Rnone,Rnone,Rore,Rnone,Rore,Rclay,Rclay,Rclay,Rclay,Rclay,Rclay,Robs,Robs,Robs,Robs,Rnone,Robs,Rgeo,Robs,Rgeo,Rnone,Rgeo,Rnone,Rore]
    -- let sol = simulate bp initrobstate initminstate inittime $ testpath

    -- let sol = simulate bp initrobstate initminstate inittime $ reverse path
    -- print sol

computeResult allbest = foldl addBPres 0 allbest
  where addBPres accu (bgeo, _, sims) = accu + (ident . bluep $ sims) * bgeo

showthebest (bgeo, bpath, sims) = do
    putStrLn $ "Blueprint number: " ++ show (bluep sims)
    putStrLn $ "Nombre de géodes ouvertes: " ++ show bgeo
    putStrLn $ "Best path: " ++ show (reverse bpath)
    mapM_ (\(t, s) -> putStr $ show t ++ "-" ++ show (length s) ++ " ") $ assocs (best sims)
    putStrLn ""

computeResult32 allbest = foldl multPres 1 allbest
  where  multPres accu (bgeo, _, _) = accu * bgeo


data Robot = Rnone | Rore | Rclay | Robs | Rgeo deriving(Eq, Show)

-- record the quantities we have
data MineralState = MinState {
      ore :: Int,
      clay :: Int,
      obsidian :: Int,
      geode :: Int
} deriving(Show)

-- record the robots we have
data RobotState = RobState {
      rore :: Int,
      rclay :: Int,
      robs :: Int,
      rgeo :: Int
} deriving(Show)


initminstate = MinState 0 0 0 0
initrobstate = RobState 1 0 0 0


data Status = Status Int RobotState MineralState

instance Show Status where
    show (Status time r m) =
      show time ++ " minutes " ++
      " Ore: " ++ show (rore r) ++ "/" ++ show (ore m) ++
      " Clay: " ++ show (rclay r) ++ "/" ++ show (clay m) ++
      " Obsidan: " ++ show (robs r) ++ "/" ++ show (obsidian m) ++
      " Geodes: " ++ show (rgeo r) ++ "/" ++ show (geode m)


-- return true if left status is inferior in all points to right status
lowerStatus (Status time r m) (Status time' r' m')
  | time /= time' = False
  | otherwise =
      rore r <= rore r' &&
      rclay r <= rclay r' &&
      robs r <= robs r' &&
      rgeo r <= rgeo r' &&
      ore m <= ore m' &&
      clay m <= clay m' &&
      obsidian m <= obsidian m' &&
      geode m <= geode m'



nextMinState robots mst =
    MinState {
      ore = ore mst + rore robots,
      clay = clay mst + rclay robots,
      obsidian = obsidian mst + robs robots,
      geode = geode mst + rgeo robots
    }

buildRobot bp robots minst Rnone = (robots, minst)

buildRobot bp robots minst Rore = (
                                robots {rore = rore robots + 1},
                                minst { ore = ore minst - rorecost bp }
                                )

buildRobot bp robots minst Rclay = (
                                 robots {rclay = rclay robots + 1},
                                 minst { ore = ore minst - rclaycost bp }
                                 )

buildRobot bp robots minst Robs = let (orec, clayc) = robscost bp in
                                (
                                robots {robs = robs robots + 1},
                                minst { ore = ore minst - orec,
                                        clay = clay minst - clayc
                                      }
                                )

buildRobot bp robots minst Rgeo = let (orec, obsc) = rgeocost bp in
                                (
                                robots {rgeo = rgeo robots + 1},
                                minst { ore = ore minst - orec,
                                        obsidian = obsidian minst - obsc
                                      }
                                )

canBuildRobot bp minst Rnone = True
canBuildRobot bp minst Rore = ore minst >= rorecost bp
canBuildRobot bp minst Rclay = ore minst >= rclaycost bp
canBuildRobot bp minst Robs =
    let (orec, clayc) = robscost bp in
    ore minst >= orec && clay minst >= clayc
canBuildRobot bp minst Rgeo =
    let (orec, obsc) = rgeocost bp in
    ore minst >= orec && obsidian minst >= obsc




simulate bp robots minst 0 _ =
    traceShow (Status 0 robots minst) $
    geode minst
simulate bp robots minst timeleft [] =
    traceShow (Status timeleft robots minst) $
    simulate bp robots minst' (timeleft-1) []
  where minst' = nextMinState robots minst

simulate bp robots minst timeleft (r:rs) =
    traceShow (Status timeleft robots minst) $
    if canBuildRobot bp minst r then trace (if r == Rnone then "Not building..." else "Building robot " ++ show r) $
                                     simulate bp robots' minst'' (timeleft-1) rs
                                else trace ("Cannot build robot " ++ show r) $
                                     simulate bp robots minst' (timeleft-1) (r:rs)
    where (robots', minst'') = simulateTurn bp robots minst r
          (_, minst') = simulateTurn bp robots minst Rnone


simulateTurn bp robots minst rob = (robots', minst'')
   where minst' = nextMinState robots minst
         (robots', minst'') = buildRobot bp robots minst' rob



-- buildchoices = reverse [Rnone, Rore, Rclay, Robs, Rgeo]
-- realbuildchoices = reverse [Rore, Rclay, Robs, Rgeo]


bruteForceBP sims path bp robots minst 0 = (geode minst, path, sims)
bruteForceBP sims path bp robots minst timeleft =
    let currentbests = (best sims)!timeleft
        currstatus = Status timeleft robots minst

        dotrace = length path < inittime - 16
        dosimupdate = length path < inittime - 9
    in

    (if dotrace
      then trace ("Trying path " ++ show (reverse path) ++
                  "\nSimState " ++ show currentbests ++
                  "\nCurrentState " ++ show currstatus)
      else id) $
    -- start by checking if we are worse that a previously seen state at 
    -- that point

    if any (lowerStatus currstatus) currentbests
      then (if dotrace 
             then trace ("Lower status, not continuing " ++ show (reverse path))
             else id)
           $
           (0, [], sims)
      else let
        sims' = if dosimupdate then updateSimuState sims currstatus
                               else sims
        initchoices = makeChoiceList bp robots minst
        choices = filter (canBuildRobot bp minst) initchoices

        tryChoice cursim r =
              let (robots', minst') = simulateTurn bp robots minst r
              in
              bruteForceBP cursim (r:path) bp robots' minst' (timeleft - 1)

        -- takeBest = maximumBy (compare `on` fst3)


        accuTryChoice (bgeo, bpath, bsims) r =
          let (geo, path, sims'') = tryChoice bsims r
          in if (geo > bgeo) then (geo, path, sims'')
                             else (bgeo, bpath, sims'')

        result = foldl accuTryChoice (0, [], sims') choices
        in result


-- inutile d'attendre sans construire de robot si
-- * on a assez de ressources pour construire au moins un robot != Rore
-- * attendre plus ne nous permettra pas de construire d'autres robots
--   (parce qu'on ne récolte pas encore les ressources nécessaires)
uselessToWaitMore bp robots minst =
    canrore && canrclay && noclay
    ||
    canrore && canrclay && canrobs && noobs
    ||
    canrore && canrclay && canrobs && canrgeo
  where
    canrore = canBuildRobot bp minst Rore
    canrclay = canBuildRobot bp minst Rclay
    noclay = rclay robots == 0

    canrobs = canBuildRobot bp minst Robs
    noobs = robs robots == 0

    canrgeo = canBuildRobot bp minst Rgeo



--
-- if we produce more mins per turn than we can consume...
--
--
makeChoiceList bp robots minst = Rgeo : (addOre . addClay . addObs) initl
   where initl = if uselessToWaitMore bp robots minst 
                   then --trace ("No use to wait more") $
                        []
                   else [Rnone]

         addObs l = if robs robots < maxobsrate bp then Robs : l else l
         addClay l = if rclay robots < maxclayrate bp then Rclay : l else l
         addOre l = if rore robots < maxorerate bp then Rore : l else l




