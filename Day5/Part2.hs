import System.IO

-- FFFFFFFLLL
-- 7x F/B ; 3x L/R
-- 2^7 ; 2^3

main = do
  fileHandle <- openFile "data/seats.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let boardingPasses = quicksort $ map parseBoardingPass ls
  print $ findMissing boardingPasses

data BoardingPass = BoardingPass { id :: Int
                                 , row :: Int
                                 , col :: Int
                                 } deriving (Show, Eq)

instance Ord BoardingPass where
  (BoardingPass a _ _) `compare` (BoardingPass b _ _) = a `compare` b

getId :: BoardingPass -> Int
getId (BoardingPass id _ _) = id

parseBoardingPass :: [Char] -> BoardingPass
parseBoardingPass "" = error "Empty string"
parseBoardingPass s
  | length s /= 10  = error "String not 10 chars"
  | otherwise       = do
    let (rows, cols) = splitAt 7 s
        row = parseRows rows
        col = parseCols cols
    BoardingPass (row * 8 + col) row col

parseRows :: [Char] -> Int
parseRows "" = error "Empty string"
parseRows r
  | length r /= 7 = error "String not 7 chars"
  | otherwise     = head $ parseRows' r [0..127]

parseRows' :: [Char] -> [Int] -> [Int]
parseRows' _ [a] = [a]
parseRows' "" _ = error "Empty string"
parseRows' s [] = error s
parseRows' s r =
  let c = head s
      midpoint = length r `div` 2 + head r
  in parseRows' (tail s) (
    if c == 'F'
      then takeWhile (< midpoint) r
      else dropWhile (< midpoint) r
    )

parseCols :: [Char] -> Int
parseCols "" = error "Empty string"
parseCols r
  | length r /= 3 = error "String not 3 chars"
  | otherwise     = head $ parseCols' r [0..7]

parseCols' :: [Char] -> [Int] -> [Int]
parseCols' _ [a] = [a]
parseCols' "" _ = error "Empty string"
parseCols' s [] = error s
parseCols' s r =
  let c = head s
      midpoint = length r `div` 2 + head r
  in parseCols' (tail s) (
    if c == 'L'
      then takeWhile (< midpoint) r
      else dropWhile (< midpoint) r
    )

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

findMissing :: [BoardingPass] -> Int
findMissing [] = error "Failed"
findMissing (b:bs) =
  let id = getId b
      nid = getId $ head bs
  in if id + 1 /= nid then id + 1 else findMissing bs