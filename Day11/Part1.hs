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

getOccupiedNeighbors :: [[Char]] -> Int -> Int -> Int 
getOccupiedNeighbors s r c =
  let neighbors = [(rr,cc) | rr <- [r-1,r,r+1], rr >= 0 && rr < length s, cc <- [c-1,c,c+1], cc >= 0 && cc < length (s !! rr), (rr,cc) /= (r,c)]
  in foldl (\acc c -> if c == '#' then acc + 1 else acc) 0 (map (uncurry (getSeat s)) neighbors)
  