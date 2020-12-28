{-# LANGUAGE MultiWayIf #-}
import System.IO
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let (Fr (s,t) (x,y)) = foldl moveFerry' (Fr (10,1) (0,0)) (parseInstructions ls)
  print $ abs x + abs y
  hClose fileHandle

data Ferry = Fr { waypoint :: (Int, Int)
                , position :: (Int, Int)
                } deriving (Show)

data Instruction = In { movement :: Char 
                      , amount :: Int 
                      } deriving (Show)

parseInstructions :: [[Char]] -> [Instruction]
parseInstructions i =
  let getMovement s = head s
      getAmount s = read (tail s) :: Int
  in [In (getMovement x) (getAmount x) | x <- i]

moveFerry' :: Ferry -> Instruction -> Ferry
moveFerry' i f = trace (show i ++ show f) moveFerry i f

moveFerry :: Ferry -> Instruction -> Ferry
moveFerry (Fr (s,t) (x,y)) (In m a)
  | m == 'R'  = if
    | a == 90   -> Fr (t,-s) (x,y)
    | a == 180  -> Fr (-s,-t) (x,y)
    | a == 270  -> Fr (-t,s) (x,y)
    | otherwise -> Fr (s,t) (x,y)
  | m == 'L'  = if
    | a == 90   -> Fr (-t,s) (x,y)
    | a == 180  -> Fr (-s,-t) (x,y)
    | a == 270  -> Fr (t,-s) (x,y)
    | otherwise -> Fr (s,t) (x,y)
  | m == 'N'  = Fr (s,t+a) (x,y)
  | m == 'S'  = Fr (s,t-a) (x,y)
  | m == 'E'  = Fr (s+a,t) (x,y)
  | m == 'W'  = Fr (s-a,t) (x,y)
  | m == 'F'  = Fr (s,t) (x+(s*a),y+(t*a))
  | otherwise = error "Invalid instruction"