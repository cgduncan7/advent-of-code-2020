import System.IO
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let ids = parseIds $ last ls
  let idsWithDelay = filter (\(i, j) -> i /= -1) $ foldl (\acc id -> acc ++ [(id, length acc)]) [] ids
  print idsWithDelay
  print $ findEarliestTimestamp idsWithDelay
  hClose fileHandle

parseIds :: String -> [Int]
parseIds "" = []
parseIds s = 
  let (first, rest) = span (/= ',') s
      cnt = if not $ null rest then parseIds $ tail rest else []
  in
    if first == "x"
      then -1 : cnt
      else (read first :: Int) : cnt

findEarliestTimestamp :: [(Int, Int)] -> Int
findEarliestTimestamp x =
  let (fi,_) = head x
      check t = all (\(i, d) -> (t + d) `mod` i == 0) x
  in head [ t | t <- [100000000000000,100000000000000+fi..], check t]

sort :: Ord a => [a] -> [a]
sort []     = []
sort (p:xs) = sort lesser ++ [p] ++ sort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
