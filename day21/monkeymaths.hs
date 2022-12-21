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

    print $ eval monkeys


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


applyOp' op (Right a) (Right b) = Right $ applyOp op a b
applyOp' op (Left a) (Left b) = error "Two unknows"

applyOp' op (Right a) (Left b) = reverseApply op a b
applyOp' op (Left a) (Right b) = reverseApply op a b


applyOp' Plus (Just a) Nothing = Left $ flip (-) a
applyOp' Plus Nothing (Just b) = Left $ flip (-) b

applyOp Plus a b = a+b
applyOp Minus a b = a-b
applyOp Mult a b = a*b
applyOp Div a b = a `div` b
applyOp Equal a b = error "Equals appearing not in equation"

