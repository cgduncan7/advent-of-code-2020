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