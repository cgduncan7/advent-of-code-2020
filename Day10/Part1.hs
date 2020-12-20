import System.IO
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let numbers = map (\l -> read l :: Int) ls
  let totalJolts = [0] ++ sort numbers ++ [maximum numbers + 3]
  let (a, _, c) = countDifferences $ computeDifferences totalJolts
  print $ show (a * c)
  hClose fileHandle

countDifferences :: [Int] -> (Int, Int, Int)
countDifferences i = countDifferences' i (0, 0, 0)

countDifferences' :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
countDifferences' [] a = a
countDifferences' (x:xs) (a,b,c)
  | x == 1    = countDifferences' xs (a+1,b,c)
  | x == 2    = countDifferences' xs (a,b+1,c)
  | x == 3    = countDifferences' xs (a,b,c+1)
  | otherwise = error "uh oh"

computeDifferences :: [Int] -> [Int]
computeDifferences (x:xs)
  | null xs   = []
  | otherwise = head xs - x : computeDifferences xs

sort :: Ord a => [a] -> [a]
sort []     = []
sort (p:xs) = sort lesser ++ [p] ++ sort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
