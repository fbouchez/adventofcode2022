import System.IO
import Data.List


main :: IO ()
main = do
    l <- parseInput
    let maximum = max_elf (-1) 0 l -- premier param: maximum jusqu'ici / deuxième param: nb de cal de l'elfe en cours
    putStrLn $ show maximum
    let thmax = three_max_elves [-1, -1, -1] 0 l
    putStrLn $ show thmax
    putStrLn $ show $ sum thmax




max_elf maximum courant [] =
    max maximum courant

max_elf maximum courant (x:xs)
  | x == -1 = max_elf (max maximum courant) 0 xs
  | otherwise = max_elf maximum (courant+x) xs



three_max_elves :: [Int] -> Int -> [Int] -> [Int]
three_max_elves lmax courant [] =
    special (courant:lmax)

three_max_elves lmax courant (x:xs)
  | x == -1 = three_max_elves (special (courant:lmax)) 0 xs
  | otherwise = three_max_elves lmax (courant+x) xs


special l = drop 1 (sort l)




parseInput :: IO [Int]
parseInput = do
    done <- isEOF
    if done
      then return []
      else do
        input_line <- getLine
        l <- parseInput
        if input_line == ""
          then
            return $ -1 : l
          else 
            let x = read input_line :: Int
            in
              return $ x:l

