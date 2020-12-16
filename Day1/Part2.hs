import System.IO

main = do
  fileHandle <- openFile "data/data.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let numbers = map read (lines fileContents)
  let (a, b, c) = findEntries numbers
  print $ a * b * c
  hClose fileHandle

findEntries :: [Int] -> (Int, Int, Int)
findEntries [] = error "No numbers provided"
findEntries nums = head [(a, b, c) | a <- nums, b <- nums, c <- nums, a + b + c == 2020]