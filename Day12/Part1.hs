{-# LANGUAGE MultiWayIf #-}
import System.IO
import Debug.Trace (trace)

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  let (Fr d (x,y)) = foldl moveFerry' (Fr 90 (0,0)) (parseInstructions ls)
  print $ abs x + abs y
  hClose fileHandle

data Ferry = Fr { direction :: Int
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
moveFerry (Fr d (x,y)) (In m a)
  | m == 'R'  = Fr (d+a) (x,y) 
  | m == 'L'  = Fr (d-a) (x,y) 
  | m == 'N'  = Fr d (x,y+a)
  | m == 'S'  = Fr d (x,y-a)
  | m == 'E'  = Fr d (x+a,y)
  | m == 'W'  = Fr d (x-a,y)
  | m == 'F'  = if
    | d `mod` 360 == 0    -> moveFerry (Fr d (x,y)) (In 'N' a)
    | d `mod` 360 == 90   -> moveFerry (Fr d (x,y)) (In 'E' a)
    | d `mod` 360 == 180  -> moveFerry (Fr d (x,y)) (In 'S' a)
    | d `mod` 360 == 270  -> moveFerry (Fr d (x,y)) (In 'W' a)
    | otherwise -> moveFerry (Fr d (x,y)) (In 'N' a)
  | otherwise = error "Invalid instruction"