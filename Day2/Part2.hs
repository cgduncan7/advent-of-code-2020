import System.IO

main = do
  fileHandle <- openFile "data/passwords.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let entries = map parseEntry $ lines fileContents
  let validPasswords = length [ e | e <- entries, evaluate e]
  print validPasswords
  hClose fileHandle



data Policy = Policy { letter :: Char
                     , frst :: Int
                     , lst :: Int
                     } deriving (Show, Read)

data Entry = Entry { policy :: Policy
                   , password :: String
                   } deriving (Show, Read)

-- `1-2 a: abc`
parseEntry :: String -> Entry
parseEntry "" = error "Entry string is empty"
parseEntry str =
  let (pol, pass) = break (== ':') str
      policy = parsePolicy pol
      password = last $ words pass
  in Entry policy password

  -- `1-2 a` --> char a, min 1, max 2
parsePolicy :: String -> Policy
parsePolicy "" = error "Policy string is empty"
parsePolicy str =
  let w = words str
      nums = head w
      char = last $ last w
      (f, l) = break (== '-') nums
      frst = read f - 1
      lst = read (tail l) - 1
  in Policy char frst lst

evaluate :: Entry -> Bool
evaluate (Entry (Policy letter frst lst) password) = 
  (password !! frst == letter) /= (password !! lst == letter)