import System.IO
import qualified Data.Maybe
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let numbers = sort $ map (\l -> read l :: Int) ls
  let totalJolts = [0] ++ numbers ++ [maximum numbers + 3]
  let adapters = getAdapters totalJolts 0
  print $ calculatePaths adapters
  hClose fileHandle

data Path = Path { adapterVoltage :: Int
                 , numPaths :: Int
                 }

data Adapter = Ad { voltage :: Int
                  , inputs :: [Int]
                  , outputs :: [Int]
                  } deriving (Show)

getAdapterVoltage :: Adapter -> Int
getAdapterVoltage (Ad v _ _) = v

getAdapterInputs :: Adapter -> [Int]
getAdapterInputs (Ad _ i _) = i

calculatePaths :: [Adapter] -> Int
calculatePaths [] = 0
calculatePaths a = calculatePaths' a []

calculatePaths' :: [Adapter] -> [(Int, Int)] -> Int
calculatePaths' [] p = snd $ last p
calculatePaths' a p =
  let aa = head a
      as = tail a
      i = getAdapterInputs aa
      s = if null i then 1 else sum $ map (Data.Maybe.fromMaybe 1 . (`lookup` p)) i
  in calculatePaths' as (p ++ [(getAdapterVoltage aa, s)])

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
