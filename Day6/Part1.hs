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
  in length (removeDuplicates (concat group)) + if null remainder then 0 else parseAnswers $ tail remainder

removeDuplicates :: String -> String
removeDuplicates "" = ""
removeDuplicates s =
  let l = last s
      i = init s
  in if l `elem` i then removeDuplicates i else l : removeDuplicates i