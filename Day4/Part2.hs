import System.IO

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID) *OPTIONAL*

main = do
  fileHandle <- openFile "data/passports.txt" ReadMode
  fileContents <- hGetContents fileHandle
  let ls = lines fileContents
  print $ checkPassports ls
  hClose fileHandle

checkPassports :: [String] -> Int
checkPassports [] = 0
checkPassports s =
  let p = takeWhile (/= "") s
      rest = dropWhile (/= "") s
      passport = words $ unwords p
      valid = if checkPassport passport then 1 else 0
  in if null rest then valid else valid + checkPassports (tail rest)

checkPassport :: [String] -> Bool
checkPassport [] = False
checkPassport a =
  let req = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
      fields = map (takeWhile (/= ':')) a
  in all (`elem` fields) req

validYear :: String -> (Int, Int) -> Bool
validYear "" _ = False
validYear s (min, max)
  | length s /= 4 = False
  | otherwise     = do
    let year = read s
    min <= year && year <= max

validBirthYear :: String -> Bool
validBirthYear s = validYear s (1920, 2002)

validIssueYear :: String -> Bool
validIssueYear s = validYear s (2010, 2020)

validExpirationYear :: String -> Bool
validExpirationYear s = validYear s (2020, 2030)

-- validHeight :: String -> Bool
-- validHeight h = HERE

