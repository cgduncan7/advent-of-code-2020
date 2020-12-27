import System.IO
import Data.Maybe ( fromMaybe,  fromJust )

-- Not the most efficient solution? Still takes some time.

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let neighborMap = createNeighborsMap ls
  print neighborMap
  let state = findStableState ls neighborMap
  print $ foldl (\acc r -> length r + acc) 0 $ map (filter (== '#')) state
  hClose fileHandle

findStableState :: [[Char]] -> [((Int, Int), [(Int, Int)])] -> [[Char]]
findStableState s n
  | s == nextState s n  = s
  | otherwise           = findStableState (nextState s n) n

nextState :: [[Char]] -> [((Int, Int), [(Int, Int)])] -> [[Char]]
nextState s n =
  let ns r c = nextSeatState s n (r,c)
      nr r = [ ns r c | c <- [0..length (s !! r) - 1] ]
  in [nr r | r <- [0..length s - 1] ]

nextSeatState :: [[Char]] -> [((Int, Int), [(Int, Int)])] -> (Int, Int) -> Char
nextSeatState s n (r, c) = 
  let seat = Data.Maybe.fromJust $ getSeat s (r, c)
      neighbors = getOccupiedNeighbors s n (r,c)
  in
    if seat == 'L' then if getOccupiedNeighbors s n (r,c) == 0 then '#' else 'L'
    else if seat == '#' then if getOccupiedNeighbors s n (r,c) >= 5 then 'L' else '#'
    else '.'

getSeat :: [[Char]] -> (Int, Int) -> Maybe Char
getSeat [] _ = error "Empty state"
getSeat s (r, c)
  | r < 0 || c < 0                      = Nothing
  | r < length s && c < length (s !! r) = Just (s !! r !! c)
  | otherwise                           = Nothing

getOccupiedNeighbors :: [[Char]] -> [((Int, Int), [(Int, Int)])] -> (Int, Int) -> Int
getOccupiedNeighbors s n (r,c) =
  let neighbors = lookupSeatNeighbors n (r,c)
      gs a = Data.Maybe.fromJust $ getSeat s a
  in foldl (\acc c -> if c == '#' then acc + 1 else acc) 0 (map gs neighbors)

lookupSeatNeighbors :: [((Int, Int), [(Int, Int)])] -> (Int, Int) -> [(Int, Int)]
lookupSeatNeighbors s l = Data.Maybe.fromJust $ l `lookup` s

createNeighborsMap :: [[Char]] -> [((Int, Int), [(Int, Int)])]
createNeighborsMap s = [ ((r,c), findSeatNeighbors s (r,c)) | r <- [0..length s - 1], c <- [0..length (head s) - 1]]

findSeatNeighbors :: [[Char]] -> (Int, Int) -> [(Int, Int)]
findSeatNeighbors s (r, c) =
  let slopes = [ (x,y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0) ]
      fn (x,y) st = do
        let xx = x * st
        let yy = y * st
        let seat = Data.Maybe.fromMaybe 'x' $ getSeat s (r+xx,c+yy)
        if seat == 'x' then (-1,-1)
        else if seat == '.' then fn (x,y) (st+1) else (r+xx,c+yy)
  in
    if Data.Maybe.fromJust (getSeat s (r,c)) /= '.' then filter (/= (-1,-1)) $ map (`fn` 1) slopes else []