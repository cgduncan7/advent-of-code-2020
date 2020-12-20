import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let numbers = map (\l -> read l :: Int) ls
  let numberToFind = numbers !! checkValues 25 0 numbers
  let contiguousNumbers = findContiguousNumbers numberToFind numbers
  print $ minimum contiguousNumbers + maximum contiguousNumbers
  hClose fileHandle

checkValues :: Int -> Int -> [Int] -> Int
checkValues preamble index list = if checkValue preamble index list
  then checkValues preamble (index + 1) list
  else index

checkValue :: Int -> Int -> [Int] -> Bool
checkValue preamble index list = (index < preamble) ||
  do
    let previousNumbers = map ((list !!) . (index -)) [1..preamble]
        sums = sumPermutations previousNumbers
        numberToCheck = list !! index
    numberToCheck `elem` sums

sumPermutations :: [Int] -> [Int]
sumPermutations (first:rest) = if null rest
  then []
  else let firstPermuations = map (+ first) rest
  in firstPermuations ++ sumPermutations rest

findContiguousNumbers :: Int -> [Int] -> [Int]
findContiguousNumbers i l = findContiguousNumbers' i l []

findContiguousNumbers' :: Int -> [Int] -> [Int] -> [Int]
findContiguousNumbers' i (h:t) a
  | sum a == i      = a
  | sum a + h <= i  = findContiguousNumbers' i t (a ++ [h])
  | otherwise       = findContiguousNumbers' i (h:t) (tail a)