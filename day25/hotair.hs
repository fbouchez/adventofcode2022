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



base = 5

main = do
    contents <- getContents
    let lns = lines contents
        snafus = map (map charToSNAFU) lns

    print snafus

    let nums = map snafuToInt snafus
    print nums
    print $ sum nums

    let test = [3..30] ++ [2022, 12345, 314159265, sum nums]

    mapM_ print $ zip test $ zip (map intToSnafu2 test) (map intToSnafuBest test)


charToSNAFU '2' = 2
charToSNAFU '1' = 1
charToSNAFU '0' = 0
charToSNAFU '-' = -1
charToSNAFU '=' = -2

snafuToChar 2 = '2'
snafuToChar 1 = '1'
snafuToChar 0 = '0'
snafuToChar (-1) = '-'
snafuToChar (-2) = '='
snafuToChar x = error $ "Unknown snafu " ++ show x



snafuToInt l = snafuToInt' 0 l

snafuToInt' acc [] = acc
snafuToInt' acc (h:rs) = snafuToInt' (acc * base + h) rs

-- snafuToInt' acc (h:rs) = sth : map (*base) acc
--
intToSnafu n = 
    let fst = until (>= n) ((+2) . (*base)) 2
        prec = fst `div` base
        twidth = (fst - prec) `div` base
    in
    (prec, fst, twidth, (n - prec) `div` twidth)


intToSnafu2 n =
    let (lo, hi) = snabounds n
        ilo = snafuToInt lo + 1
        ihi = snafuToInt hi + 1
        rest = n - ilo
        lst = continue rest ihi ilo [head hi]
    in
    -- traceShow (n, lst, ilo, ihi) $
    reverse $ map snafuToChar $ lst

        -- rng = (ihi - ilo) `div` base
        -- tranche = (n - ilo) `div` rng
        -- rest = (n - ilo) - rng * tranche
    -- in
    -- (n, (head hi, tranche - 2, rng, ihi, rest))




--
--
-- This is the real function, that uses math and mind...
--
--
intToSnafuBest n =
    intToSnafuBest' n []

intToSnafuBest' 0 lst = map snafuToChar $ lst
intToSnafuBest' n lst = intToSnafuBest' n' $ u:lst
  where n' = (n-u) `div` base
        u' = n `mod` base
        u = (u' + 2) `mod` base -2



continue 0 _ _ l = l
continue rem ihi ilo l = -- traceShow rem $
  let rng = (ihi - ilo) `div` base
      tranche = rem `div` rng
  in
  continue (rem - rng * tranche) 
  (rng * (tranche+1))
  (rng * tranche)
  $ (tranche-2):l



snabounds n = (down, up)
  where up = until ((>=n) . snafuToInt) nextpow [1]
        down = prevpow up


nextpow (1:rs) = 2:rs
nextpow (2:rs) = 1:2:rs

prevpow (2:rs) = 1:rs
prevpow (1:rs) = rs




-- 22 = 12
-- 1== = 25 - 10 - 2
