import System.IO

main = do
  fileHandle <- openFile "data/passwords.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let entries = map parseEntry $ lines fileContents
  let validPasswords = length [ e | e <- entries, evaluate e]
  print validPasswords
  hClose fileHandle


data Policy = Policy { letter :: Char
                     , min :: Int
                     , max :: Int
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
      (mn, mx) = break (== '-') nums
      min = read mn
      max = read $ tail mx
  in Policy char min max

evaluate :: Entry -> Bool
evaluate (Entry (Policy letter min max) password) = 
  let occurrences = sum [ 1 | c <- password, c == letter ]
  in min <= occurrences && occurrences <= max