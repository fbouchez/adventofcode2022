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

key =  811589153

main = do
    contents <- getContents
    let nums = map read $ lines contents :: [Int]
        nb = length nums

        numsdec = map ((*) key) nums

        -- numsidx = zip nums [1..]
        numsidx = zip numsdec [1..]

    -- let finallist = foldl (movenum nb) numsidx [1..nb]

    let oneround list _ = foldl (movenum nb) list [1..nb]
        tenrounds list = foldl oneround list [1..10]

        finallist = tenrounds numsidx


    print numsidx
    print finallist

    let zero = arrangezero finallist
    
    print zero

    let (a,b,c) = findCoordinates zero

    print (a,b,c)

    putStrLn $ "Result: " ++ show (a+b+c)





movenum nb numsidx n =
    -- first find the right number
    let (deb, (e,x) :fin) = span ((/= n) . snd) numsidx
    in
    -- traceShow (deb, (e,x), fin) $
    -- assert x == n $
    let e' = if abs e <= (nb - 1) then e
                                  else e `mod` (nb-1)

    in
    let numidx' = if e' > 0 
          then
                let lst = fin ++ deb
                    (deb', fin') = splitAt e' lst
                in deb' ++ ((e,x) : fin')
          else
                let lst = fin ++ deb
                    (deb', fin') = splitAt ((nb-1) + e') lst
                in deb' ++ ((e,x) : fin')
    in
      -- trace (myshow numidx') $
      numidx'


myshow n = show $ fst $ unzip n

arrangezero list = 
    let (deb, (e:fin)) = span (/= 0) $ fst . unzip $ list
    in 
    e:fin ++ deb



findCoordinates zero = (get 1000, get 2000, get 3000)
    where l = length zero
          get n = zero!!(n `mod` l)



