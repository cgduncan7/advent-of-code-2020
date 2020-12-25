import System.IO
import qualified Data.Maybe
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/small.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let numbers = sort $ map (\l -> read l :: Int) ls
  let totalJolts = [0] ++ numbers ++ [maximum numbers + 3]
  let adapters = getAdapters totalJolts 0
  -- print $ show adapters
  print $ removeRedundantAdapters adapters
  hClose fileHandle

data Path = Path { adapter :: Adapter
                 , paths :: Int
                 }

-- calculatePaths :: [Adapter] -> [Path]
-- calculatePaths [] = []
-- calculatePaths

removeRedundantAdapters :: [Adapter] -> [Adapter]
removeRedundantAdapters a =
  let e = last a
      r = init a
      i = filter (\(Ad n _ _) -> n `notElem` getAdapterInputs e) a
  in i

data Adapter = Ad { voltage :: Int
                  , inputs :: [Int]
                  , outputs :: [Int]
                  } deriving (Show)

getAdapterVoltage :: Adapter -> Int
getAdapterVoltage (Ad v _ _) = v

getAdapterInputs :: Adapter -> [Int]
getAdapterInputs (Ad _ i _) = i

getAdapterOutputs :: Adapter -> [Int]
getAdapterOutputs (Ad _ _ o) = o

getAdapters :: [Int] -> Int -> [Adapter]
getAdapters [] _ = []
getAdapters j i = if i >= length j then [] else
  let cj = j !! i
      op = takeWhile (<=cj+3) $ dropWhile (<=cj) j
      ip = dropWhile (<cj-3) $ takeWhile (<cj) j
  in Ad cj ip op : getAdapters j (i+1)

sort :: Ord a => [a] -> [a]
sort []     = []
sort (p:xs) = sort lesser ++ [p] ++ sort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
