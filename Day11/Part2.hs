import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let state = findStableState ls
  print $ foldl (\acc r -> length r + acc) 0 $ map (filter (== '#')) state
  hClose fileHandle

findStableState :: [[Char]] -> [[Char]]
findStableState s
  | s == nextState s  = s
  | otherwise         = findStableState $ nextState s

nextState :: [[Char]] -> [[Char]]
nextState s =
  let ns r c = nextSeatState s r c
      nr r = [ ns r c | c <- [0..length (s !! r) - 1] ]
  in [nr r | r <- [0..length s - 1] ]

nextSeatState :: [[Char]] -> Int -> Int -> Char
nextSeatState s r c = 
  let seat = getSeat s r c
      neighbors = getOccupiedNeighbors s r c
  in
    if seat == 'L' then if getOccupiedNeighbors s r c == 0 then '#' else 'L'
    else if seat == '#' then if getOccupiedNeighbors s r c >= 4 then 'L' else '#'
    else '.'

getSeat :: [[Char]] -> Int -> Int -> Char
getSeat [] _ _ = error "Empty state"
getSeat s r c
  | r < length s && c < length (s !! r) = s !! r !! c
  | otherwise                           = error "Invalid seat"

-- needs to find first seat in each of 8 directions
createNeighborsMap :: [[Char]] -> [((Int, Int), [(Int, Int)])]
createNeighborsMap _ = []

getRowNeighbors :: [Char] -> [((Int, Int), [(Int, Int)])]
getRowNeighbors c = []

getSeatNeighbors :: [[Char]] -> (Int, Int) -> [(Int, Int)]
getSeatNeighbors s (r, c) =
  