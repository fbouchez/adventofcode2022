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


-- inittime = 10 -- for testing purposes
inittime = 24 -- for testing purposes
-- inittime = 24

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
          maxclayrate = obsidcostClay
          maxobsrate = geodecosteObs


main = do
    contents <- getContents
    let blueprintsraw = readP_to_S parseBluePrints contents
        blueprints = fst . head $ blueprintsraw

    print blueprintsraw
    print blueprints

    let bp = blueprints!!1


    -- let testBuy = [Rclay, Rclay, Rclay, Robs, Rclay, Robs, Rgeo, Rgeo]
        -- sol = simulate (head blueprints) initrobstate initminstate inittime testBuy
    -- print sol

    -- let (numgeodes, path) = bruteForceBP [] (head blueprints) initrobstate initminstate inittime
    let (numgeodes, path) = bruteForceBP [] bp initrobstate initminstate inittime

    -- putStrLn $ "Nombre de géodes ouvertes: " ++ show numgeodes
    -- putStrLn $ "Simulating path: " ++ show (reverse path)

    let testpath = [Rnone,Rnone,Rore,Rnone,Rore,Rclay,Rclay,Rclay,Rclay,Rclay,Rclay,Robs,Robs,Robs,Robs,Rnone,Robs,Rgeo,Robs,Rgeo,Rnone,Rgeo,Rnone,Rore]

    let sol = simulate bp initrobstate initminstate inittime $ testpath
    -- let sol = simulate bp initrobstate initminstate inittime $ reverse 
    -- path
    print sol



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



buildchoices = reverse [Rnone, Rore, Rclay, Robs, Rgeo]
realbuildchoices = reverse [Rore, Rclay, Robs, Rgeo]


bruteForceBP path bp robots minst 0 = (geode minst, path)
bruteForceBP path bp robots minst timeleft =
    (if length path < 10 then trace ("Trying path " ++ show (reverse path))
                        else id)
    takeBest $ tryAllPossibleChoices
  where tryAllPossibleChoices = map tryChoice choices

        initchoices = if uselessToWaitMore bp robots minst
                        then trace ("No use to wait more") $
                             realbuildchoices
                        else buildchoices

        choicesa = filter (canBuildRobot bp minst) initchoices

        -- choices = filter tooMuchMins choisesa
        choices = choicesa

        tryChoice r =
          let (robots', minst') = simulateTurn bp robots minst r
          in
          bruteForceBP (r:path) bp robots' minst' (timeleft - 1)

        takeBest = maximumBy (compare `on` fst)


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
-- tooMuchMins = 
