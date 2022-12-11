import AoC
import Data.List
import Text.ParserCombinators.ReadP

main = do
    contents <- getContents
    let parseresults = readP_to_S parseRanges contents
        rangelist = fst . last $ parseresults
    print rangelist
    let inters = uncurry checkRngIntersec <$> rangelist
    print inters
    putStrLn $ "Number of inclusions:" ++ (show $ length . filter (== Inclusion) $ inters)
    putStrLn $ "Number of strict overlaps:" ++ (show . length . filter (== Overlap) $ inters)
    putStrLn $ (++) "Number of all overlaps:" $ show . length . filter (/= None) $ inters

data RngIntersect = Inclusion | Overlap | None deriving (Eq, Show)

checkRngIntersec a b =
    let [(x,y),(x',y')] = sortBy fstsnd [a,b]
    in if y < x' then None
                 else if y' <= y then Inclusion
                                 else Overlap
fstsnd (x,y) (x',y')
  | x == x' = compare y' y
  | otherwise = compare x x'

type Range = (Int, Int)

parseRanges:: ReadP [(Range, Range)]
parseRanges = sepBy1 twoRngCpl (char '\n')

twoRngCpl = do
    f <- dashRange
    char ','
    s <- dashRange
    return (f,s)
