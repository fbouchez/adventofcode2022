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


data Operator = Plus | Minus | Mult | Div | Equal deriving(Show)

chToOp '+' = Plus
chToOp '-' = Minus
chToOp '*' = Mult
chToOp '/' = Div
chToOp '=' = Equal


data Action = Yell Int | Unknown | Operation String Operator String deriving(Show)

data Monkey = Monkey String Action deriving(Show)

parseMonkeys = do
    monkeys <- endBy1 parseMonkey skipSpaces
    eof
    return monkeys


parseMonkey = do
    name <- parseIdent
    char ':'
    skipSpaces
    action <- parseAction
    return $ (name, Monkey name action)

parseAction = parseYell <++ parseOperation

parseYell = number >>= return . Yell

parseOperation = do
    f <- parseIdent
    skipSpaces
    op <- parseOp
    skipSpaces
    s <- parseIdent
    return $ Operation f op s


parseOp = do 
    c <-satisfy (flip elem "+-*/")
    return $ chToOp c





main = do
    contents <- getContents
    let monkeysp = readP_to_S parseMonkeys contents
        monkeys = fst . head $ monkeysp

    print monkeys

    putStrLn "First part: "
    print $ eval monkeys
    putStrLn "Second part: "
    print $ eval2 monkeys


eval monkeys =
    let r = fromJust $ lookup "root" monkeys
    in
    eval' monkeys r


eval' :: [(String, Monkey)] -> Monkey -> Int
eval' ms (Monkey name (Yell n)) = n
eval' ms (Monkey name (Operation f o s)) = applyOp o fn sn
  where fn = eval' ms fm
        sn = eval' ms sm
        fm = fromJust $ lookup f ms
        sm = fromJust $ lookup s ms


applyOp op a b = trace ("Applying " ++ show (op, a, b)) $ applyOp' op a b

applyOp' Plus a b = a+b
applyOp' Minus a b = a-b
applyOp' Mult a b = a*b
applyOp' Div a b = a `safediv` b
applyOp' Equal a b = error "Equals appearing not in equation"


eval2 monkeys =
    let Monkey _ (Operation a _ b) = fromJust $ lookup "root" monkeys
        ae = eval2' monkeys am
        be = eval2' monkeys bm
        am = fromJust $ lookup a monkeys
        bm = fromJust $ lookup b monkeys
    in
      resolveEquation ae be


resolveEquation (Right a) (Right b) = error "No unknown"
resolveEquation (Left a) (Left b) = error "Two unknows"
resolveEquation (Right a) (Left f) = f a
resolveEquation (Left f) (Right b) = f b

eval2' :: [(String, Monkey)] -> Monkey -> Either (Int -> Int) Int
eval2' ms (Monkey "humn" _) = Left id
eval2' ms (Monkey name (Yell n)) = Right n
eval2' ms (Monkey name (Operation f o s)) = applyOp2 o fn sn
  where fn = eval2' ms fm
        sn = eval2' ms sm
        fm = fromJust $ lookup f ms
        sm = fromJust $ lookup s ms


applyOp2 op (Right a) (Right b) = Right $ applyOp op a b
applyOp2 op (Left a) (Left b) = error "Two unknows"

-- r == a + x => x == r-a
applyOp2 Plus (Right a) (Left f) = Left $ f . flip (-) a
applyOp2 Plus (Left f) (Right b) = Left $ f . flip (-) b

-- r == a - x => x == a-r
applyOp2 Minus (Right a) (Left f) = Left $ f . (-) a
-- r == x - b => x == b+r
applyOp2 Minus (Left f) (Right b) = Left $ f . (+) b

-- r == a * x => x == r // a
applyOp2 Mult (Right a) (Left f) = Left $ f . flip safediv a
applyOp2 Mult (Left f) (Right b) = Left $ f . flip safediv b

-- r == a // x => x == a // r
applyOp2 Div (Right a) (Left f) = Left $ f . safediv a
-- r == x // b => x == r * b
applyOp2 Div (Left f) (Right b) = Left $ f . (*) b


safediv a b = 
    trace ("Making division " ++ show (a, b)) $
    assert (a `mod` b == 0) $ a `div` b
