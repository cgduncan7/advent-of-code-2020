import System.IO

main = do
  fileHandle <- openFile "data/answers.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  print $ parseAnswers ls
  hClose fileHandle

parseAnswers :: [String] -> Int
parseAnswers [] = 0
parseAnswers s =
  let group = takeWhile (/= "") s
      remainder = dropWhile (/= "") s
  in keepDuplicates group + if null remainder then 0 else parseAnswers $ tail remainder

keepDuplicates :: [String] -> Int
keepDuplicates [] = 0
keepDuplicates s =
  let l = head s
      r = tail s
  in length $ filter (\c -> all (elem c) r) l