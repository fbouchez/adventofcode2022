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



main = do
    grid <- getCharMap

    let entry = (1, 2)
        ((_,_), (height, width)) = bounds grid
        exit = (height, width-1)

    putStrLn $ showCharMap grid


    let numsteps = searchPath grid
    putStrLn $ "Steps: " ++ show numsteps


searchPath grid = until exitReached oneStep (0, grid)

oneStep (n, g) = (n+1, g)

exitReached = const True




