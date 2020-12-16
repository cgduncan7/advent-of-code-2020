import System.IO

main = do
  fileHandle <- openFile "data/map.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let rows = length ls
  let xslope = 3
  let yslope = 1
  print $ sum [ 1 | r <- [0..(rows - 1)], checkForTree ls (r * xslope, r * yslope)]
  hClose fileHandle

-- returns True if tree
checkForTree :: [String] -> (Int, Int) -> Bool
checkForTree [] _ = error "Rows are empty"
checkForTree r (x, y)
  | x < 0                 = error "Index is invalid"
  | y < 0                 = error "Index is invalid"
  | y >= length r         = error "Index is invalid"
  | x >= length (r !! y)  = checkForTree r (x `mod` length (r !! y), y) -- handles repetition
  | otherwise             = r !! y !! x == '#'