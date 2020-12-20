import System.IO

main = do
  fileHandle <- openFile "data/short.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  parseInstructions ls
  hClose fileHandle

data Instruction = Ins { id :: Int
                       , op :: String
                       , arg :: Int
                       } deriving (Show)

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions s = foldr (\acc x -> parseInstruction acc x) 0 s

parseInstruction :: Int -> String -> Instruction
parseInstruction _ "" = error "Empty string"
parseInstruction i s =
  let (o,a) = span (/= ' ') s
      b = read $ dropWhile (`elem` [' ', '+']) a :: Int
  in Ins i o b