import AoC
import Data.List
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP

--
-- Note: this is a way of dealing with today's problem staying pure and 
-- without resorting to ST monad. However, it is not very efficient as we 
-- need each time to go fish for the stack we want in the stack list and we 
-- recreate the stack each time (no side-effect allowed).
--
-- The last print shows an example of how to better solve this problem 
-- using Data.Sequence (that allows update, remaining pure).
--
--

main = do
    contents <- getContents
    let l@[tstacks,rawcommands] = T.splitOn (T.pack "\n\n") $ T.pack contents
        -- get the data in the 'correct' direction
        lstacks = T.transpose . T.lines $ tstacks
        rstacks = removeStackTrash $ lstacks
        stacks = T.unpack <$> T.dropWhile (==' ') <$> rstacks

        seqstacks = S.fromList $ "":stacks

        -- now get the commands as triplets of ints
        commands = parseCommand <$> T.unpack <$> T.lines rawcommands

    putStrLn "Stacks:"
    print stacks
    putStrLn "Commands:"
    print commands

    let final = foldl applyCommand stacks commands
    print final
    putStrLn "First part:"
    print $ head <$> final

    let final' = foldl applyCommand' stacks commands
    print final'
    putStrLn "Second part:"
    print $ head <$> final'

    let seqfinal = foldl applySeqCommand seqstacks commands
    print seqfinal
    putStrLn "Second part with sets:"
    print $ head <$> S.drop 1 seqfinal



removeStackTrash :: [a] -> [a]
removeStackTrash [_,l,_] = [l]
removeStackTrash (_:l:_:_:rs) = l : (removeStackTrash rs)

parseCommand :: String -> (Int, Int, Int)
parseCommand cmd = (a,b,c)
  where [_,sa,_,sb,_,sc] = words cmd
        [a,b,c] = read <$> [sa,sb,sc]


-- apply command for first part (one crate at a time)
applyCommand stacks (0,f,t) = traceShow (stacks) $ stacks
applyCommand stacks (n,f,t) = traceShow (stacks, (n,f,t)) $
    applyCommand (putOne x t st') (n-1, f, t)
  where (x, st') = getOne f stacks

-- apply command for second part (may crates at a time)
applyCommand' stacks (n,f,t) = traceShow (stacks, (n,f,t)) $
    putSome x t st'
  where (x, st') = getSome n f stacks


-- get down the rabbit hole of lists to extract the head of the one we want
getOne 1 (s:sts) = (head s, tail s : sts)
getOne n (s:sts) = (x, s : sts')
  where (x, sts') = getOne (n-1) sts

-- get down the rabbit hole of lists to push on the one we want
putOne x 1 (s:sts) = ((x:s):sts)
putOne x n (s:sts) = s:(putOne x (n-1) sts)


getSome x 1 (s:sts) = (h, t: sts)
  where (h,t) = splitAt x s
getSome x n (s:sts) = (h, s : sts')
  where (h, sts') = getSome x (n-1) sts

putSome x 1 (s:sts) = ((x++s):sts)
putSome x n (s:sts) = s:(putSome x (n-1) sts)



applySeqCommand ss (n,f,t) = --traceShow (ss, (n,f,t)) $
    S.update f st' $ S.update t targ' ss
  where st = S.index ss f
        (x,st') = splitAt n st
        targ = S.index ss t
        targ' = x ++ targ


