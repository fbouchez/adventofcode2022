import AoC
import Debug.Trace

data RockPaperScissors = R | P | S deriving(Eq)

instance Show RockPaperScissors where
    show R = "Rock"
    show P = "Paper"
    show S = "Scissors"

nextShape R = P
nextShape P = S
nextShape S = R

prevShape R = S
prevShape P = R
prevShape S = P

instance Read RockPaperScissors where
  readsPrec _ c =
    let (s, r) = span (/= ' ') c
    in case s of
         "A" -> [(R, r)]
         "B" -> [(P, r)]
         "C" -> [(S, r)]
         "X" -> [(R, r)]
         "Y" -> [(P, r)]
         "Z" -> [(S, r)]


data WinLose = Win | Draw | Lose deriving(Eq, Show)

instance Read WinLose where
  readsPrec _ c =
    let (s, r) = span (/= ' ') c
    in case s of
         "X" -> [(Lose, r)]
         "Y" -> [(Draw, r)]
         "Z" -> [(Win, r)]



main = do
    contents <- getContents
    let lns = fmap splitSpace . lines $ contents
        fstpart = fmap (fmap read) lns :: [[ RockPaperScissors ]]
        points = foldl countPoints 0 fstpart

    -- print fstpart
    print points

    let secondpart = fmap (rcpl . couple) lns
        rcpl (x,y) = (read x, read y) :: (RockPaperScissors, WinLose)
        sndpoints = foldl countSecPoints 0 secondpart

    -- print secondpart
    print sndpoints


theround R P = Win
theround P S = Win
theround S R = Win
theround x y
  | x == y = Draw
  | otherwise = Lose


points R = 1
points P = 2
points S = 3

countPoints c [x,y] = traceShow (c, x, y, theround x y) $ c + (points y) + (rpoints $ theround x y)

rpoints Win = 6
rpoints Draw = 3
rpoints Lose = 0

countSecPoints c (x,y) = c + points mychoice + (rpoints $ theround x mychoice)
  where mychoice = chooseShape x y


chooseShape x Draw = x
chooseShape x Win = nextShape x
chooseShape x Lose = prevShape x


