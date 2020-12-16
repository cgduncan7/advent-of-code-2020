import System.IO

main = do
  fileHandle <- openFile "data/data.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let numbers = map read (lines fileContents)
  let (a, b) = findEntries numbers
  print $ a * b
  hClose fileHandle

findEntries :: [Int] -> (Int, Int)
findEntries [] = error "No numbers provided"
findEntries (a:as) =
  let add y = a + y == 2020
      filtered = filter add as
  in if length filtered == 1 then (a, head filtered) else findEntries as
      