import Data.List
import System.IO
import Debug.Trace

main = do
  packet <- getLine
  print $ analyse 4 0 packet
  print $ analyse 14 0 packet

analyse n x lst@(_:rs) =
    let deb = take n lst
        l = length $ nub $ sort deb
    in
    -- traceShow (l,deb) $
    if l == n then
      x+n
    else
      analyse n (x+1) rs
