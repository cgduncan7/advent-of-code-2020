import System.IO

main = do
  fileHandle <- openFile "data/data.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  print ls
  hClose fileHandle