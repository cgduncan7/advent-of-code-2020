import System.IO

main = do
  fileHandle <- openFile "data/map.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let rows = length ls
  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  print $ product $ map (\(xs, ys) -> sum [ 1 | r <- [0..(rows - 1)], r * ys < rows , checkForTree ls (r * xs, r * ys) ]) slopes
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