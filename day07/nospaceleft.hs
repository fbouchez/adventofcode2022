import AoC
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP

data DirEntry = Directory (String, [DirEntry]) | File (Int, String) deriving (Show)

data Command = ListDir | GoDown String | GoUp deriving (Show)

main = do
    contents <- getContents
    let lst = readP_to_S parseAllFS contents
        cmdfs = fst . head $ lst

        (smalldirs, alldirs) = explore100000 cmdfs
    print $ smalldirs
    print $ alldirs

    putStr "Sum of sizes of 'small' directories "
    print $ sum . map snd $ smalldirs
    putStr "Smallest directory of enough size "
    print $ findBestCandidate alldirs

    putStrLn "WARNING: NOT WORKING PROGRAM AS IF MULTIPLE DIRS HAVE THE SAME NAME IN THE HIERARCHY, WE DO NOT DIFFERENTIATE !!! (so this is probably where the discrepancy is coming from"


parseAllFS = do
    fs <- many1 parseFS
    eof
    return fs


parseFS = do
    cmds <- endBy1 parseCmd skipSpaces
    dircontents <- endBy1 (parseFile <++ parseDir) skipSpaces
    return (cmds, dircontents)

-- mySpaces = many1 (satisfy is_a_space)
mySpaces = (satisfy is_a_space)

myLineReturn = (satisfy is_line_return)

is_a_space c = trace ("Space testing " ++ show c) $ isSpace c

is_line_return c = trace ("Lineret ? " ++ show c) $ c == '\n'


parseCmd = do
    char '$'
    skipSpaces
    s <- look
    parseCD <++ (string "ls" >> return ListDir)


parseCD = do
  string "cd "
  fn <- parseFileName
  if fn == ".." then return GoUp
                else return $ GoDown fn


parseDir :: ReadP (DirEntry)
parseDir = do
    string "dir"
    skipSpaces
    fn <- parseFileName
    return $ Directory (fn, [])

parseFile :: ReadP (DirEntry)
parseFile = do
    i <- number
    skipSpaces
    f <- parseFileName
    return $ File (i, f)


-- parseFileName = many1 (satisfy isAlnumDot)
parseFileName = munch1 isAlnumDot

isAlnumDot c =
    isAlphaNum c || c == '.' || c == '/'


explore100000 cmdfs =
    foldr scancmdfs ([], []) cmdfs


scancmdfs (cmds, dircont) (smalllist, dictionnary) =
    let GoDown dir = fromJust $ find isGoDown cmds
    in
    trace ("The dir is " ++ dir) $
    let dirsize = diskUsage dircont dictionnary
    in
    (
      if dirsize < 100000 then (dir, dirsize) : smalllist
                          else smalllist
      ,
      (dir, dirsize) : dictionnary
    )

  where isGoDown (GoDown _) = True
        isGoDown _ = False


diskUsage dircont dict = foldl (addsize dict) 0 dircont
  where addsize dict accu (File (i, _)) = accu + i
        addsize dict accu (Directory (dirname, _)) = accu + dirsize dirname
          where dirsize d = fromJust $ lookup d dict


findBestCandidate :: [(String, Int)] -> (String, Int)
findBestCandidate alldirs = minimumBy (compare `on` snd) $ filter sizeenough alldirs
  where sizeenough (_, size) = 
          size >= requiredspace
        requiredspace = let rs = 30000000 - unusedspace in
          trace ("Need " ++ show rs ++ " of memory") $ rs
        unusedspace = 70000000 - totalusedspace
        totalusedspace = fromJust $ lookup "/" alldirs
        -- totalusedspace = case (lookup "/" alldirs) of
                           -- Nothing -> error "Could not find root"
                           -- Just i -> i


