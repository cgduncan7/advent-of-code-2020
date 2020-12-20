import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  print $ evaluateInstructions $ parseInstructions ls
  hClose fileHandle

data Instruction = Ins { id :: Int
                       , op :: String
                       , arg :: Int
                       } deriving (Show)

getInstructionId :: Instruction -> Int
getInstructionId (Ins id _ _) = id

getInstructionOp :: Instruction -> String
getInstructionOp (Ins _ op _) = op

getInstructionArg :: Instruction -> Int
getInstructionArg (Ins _ _ arg) = arg

data ProgramState = PS { pc :: Int
                       , acc :: Int
                       , instructions :: [Int]
                       , halt :: Bool
                       } deriving (Show)

getProgramStatePC :: ProgramState -> Int
getProgramStatePC (PS pc _ _ _) = pc

getProgramStateAcc :: ProgramState -> Int
getProgramStateAcc (PS _ acc _ _) = acc

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions (s:ss) = parseInstruction 0 s : parseInstructions' 1 ss

parseInstructions' :: Int -> [String] -> [Instruction]
parseInstructions' _ [] = []
parseInstructions' i (s:ss) = parseInstruction i s : parseInstructions' (i+1) ss

parseInstruction :: Int -> String -> Instruction
parseInstruction _ "" = error "Empty string"
parseInstruction i s =
  let (o,a) = span (/= ' ') s
      b = read $ dropWhile (`elem` [' ', '+']) a :: Int
  in Ins i o b

evaluateInstructions :: [Instruction] -> ProgramState
evaluateInstructions [] = PS 0 0 [] False
evaluateInstructions is = evaluateInstructions' (PS 0 0 [] False) is

evaluateInstructions' :: ProgramState -> [Instruction] -> ProgramState
evaluateInstructions' p [] = p
evaluateInstructions' (PS a b c True) _ = PS a b c True
evaluateInstructions' p i = evaluateInstructions' (evaluateInstruction p (i !! getProgramStatePC p)) i

evaluateInstruction :: ProgramState -> Instruction -> ProgramState
evaluateInstruction (PS pc acc is h) (Ins id op arg) = if id `elem` is
  then PS id acc is True
  else PS (pc + if op == "jmp" then arg else 1) (acc + if op == "acc" then arg else 0) (is ++ [id]) False