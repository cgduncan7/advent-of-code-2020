import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let wait = read $ head ls :: Int
  let ids = parseIds $ last ls
  let (minId, minWait) = findEarliestTimestamp wait ids
  print $ (minWait  - wait) * minId
  hClose fileHandle

parseIds :: String -> [Int]
parseIds "" = []
parseIds s = 
  let (first, rest) = span (/= ',') s
      cnt = if not $ null rest then parseIds $ tail rest else []
  in
    if first == "x"
      then cnt
      else (read first :: Int) : cnt

findEarliestTimestamp :: Int -> [Int] -> (Int, Int)
findEarliestTimestamp w is =
  let ts = [(i, i * ((w `div` i) + 1)) | i <- is]
  in foldr (\(mi, mw) (ti, tw) -> if tw < mw then (ti, tw) else (mi, mw)) (0, maxBound :: Int) ts