import AoC
import Debug.Trace
import Data.Char
import Data.List
import Control.Exception

main = do
    contents <- getContents
    let lns = lines contents
        result = foldl computeSack 0 lns
    print result
    let result2 = foldl compute3Sack 0 $ group3 lns
    print result2


computeSack acc l = traceShow (acc, l) $ acc + (itemWorth i)
   where len = length l
         (fst,snd) = splitAt (len `div` 2) l
         int = intersect fst snd
         i = traceShow int $ assert ((== 1) . length . nub $ int) $ head int


itemWorth :: Char -> Int
itemWorth c = assert (isLetter c) $
  if isLower c then ord(c) - ord('a') + 1
               else ord(c) - ord('A') + 27


group3 :: [String] -> [(String, String, String)]
group3 [] = []
group3 (a:b:c:rs) = (a,b,c) : group3 rs
group3 _ = error "Not a multiple of 3"

compute3Sack acc (a,b,c) = acc + (itemWorth i)
  where i2 = intersect a b
        i3 = intersect i2 c
        i = traceShow i3 $ assert ((== 1) . length . nub $ i3) $ head i3
