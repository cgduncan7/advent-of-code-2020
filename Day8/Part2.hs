import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let instructions = parseInstructions ls
  let loopingPS = evaluateInstructions instructions
  let usedJmpAndNop = filter (\f -> getInstructionOp f /= "acc") $ map (instructions !!) (getProgramStateInstructions loopingPS)
  let bruteForceBaby = map (\i -> takeWhile (\b -> getInstructionId b < getInstructionId i) instructions ++ [Ins (getInstructionId i) (if getInstructionOp i == "jmp" then "nop" else "jmp") (getInstructionArg i)] ++ dropWhile (\a -> getInstructionId a <= getInstructionId i) instructions) usedJmpAndNop
  print $ filter (\(PS _ _ _ _ c) -> c) $ map evaluateInstructions bruteForceBaby
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
                       , completed :: Bool
                       } deriving (Show)

getProgramStatePC :: ProgramState -> Int
getProgramStatePC (PS pc _ _ _ _) = pc

getProgramStateAcc :: ProgramState -> Int
getProgramStateAcc (PS _ acc _ _ _) = acc

getProgramStateInstructions :: ProgramState -> [Int]
getProgramStateInstructions (PS _ _ i _ _) = i

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
evaluateInstructions [] = PS 0 0 [] False False
evaluateInstructions is = evaluateInstructions' (PS 0 0 [] False False) is

evaluateInstructions' :: ProgramState -> [Instruction] -> ProgramState
evaluateInstructions' p [] = p
evaluateInstructions' (PS a b c True d) _ = PS a b c True d
evaluateInstructions' (PS a b c d True) _ = PS a b c d True
evaluateInstructions' (PS p a i h c) is = if p >= length is
  then PS p a i h True
  else evaluateInstructions' (evaluateInstruction (PS p a i h c) (is !! p)) is

evaluateInstruction :: ProgramState -> Instruction -> ProgramState
evaluateInstruction (PS pc acc is h c) (Ins id op arg) = if id `elem` is
  then PS id acc is True False
  else PS (pc + if op == "jmp" then arg else 1) (acc + if op == "acc" then arg else 0) (is ++ [id]) False False