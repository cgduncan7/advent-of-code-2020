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
  let req = [("byr", validBirthYear),("iyr", validIssueYear),("eyr", validExpirationYear),("hgt", validHeight),("hcl", validHairColor),("ecl", validEyeColor),("pid", validPassportID)]
      getField p = takeWhile (/= ':') p
      getData p = tail $ dropWhile (/= ':') p
      ps = map (\p -> (getField p, getData p)) a
  in all (\(f, v) -> maybe False v (lookup f ps)) req

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

validHeight :: String -> Bool
validHeight "" = False
validHeight a =
  let (n, unit) = splitAt (length a - 2) a
      measure = read n :: Int
      validUnit = unit == "in" || unit == "cm"
      validMeasurement = if unit == "in"
                         then 59 <= measure && measure <= 76
                         else 150 <= measure && measure <= 193
  in ((validUnit && measure > 0) && validMeasurement)

validHairColor :: String -> Bool
validHairColor "" = False
validHairColor (h:s)
  | h == '#'  = do
    let valid = "abcdefABCDEF0123456789"
        isValid a = a `elem` valid
    all isValid s
  | otherwise = False

validEyeColor :: String -> Bool
validEyeColor c = c `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

validPassportID :: String -> Bool
validPassportID "" = False
validPassportID s =
  let valid = "0123456789"
  in length s == 9 && all (`elem` valid) s