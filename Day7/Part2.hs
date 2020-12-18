import System.IO

main = do
  fileHandle <- openFile "data/input.txt" ReadMode
  fileContent <- hGetContents fileHandle
  let ls = map init $ lines fileContent -- remove stupid period
  let rules = map parseRule ls
  print $ getBagsWithin "shiny gold" rules
  hClose fileHandle

type Color = String

data Content = Content { contentColor :: Color
                       , quantity :: Int
                       } deriving (Show)

getContentColor :: Content -> Color
getContentColor (Content c _) = c

getContentQuantity :: Content -> Int
getContentQuantity (Content _ q) = q

data Rule = Rule { ruleColor:: Color
                 , inner :: [Content]
                 } deriving (Show)

contains :: Rule -> Color -> Bool
contains (Rule _ i) c = any (\d -> getContentColor d == c) i

getRuleColor :: Rule -> Color
getRuleColor (Rule c _) = c

getRuleContents :: Rule -> [Content]
getRuleContents (Rule _ i) = i

parseRule :: String -> Rule
parseRule "" = error "Empty string"
parseRule r =
  let w = words r
      (o, c) = span (/= "contain") w
      d = tail c
  in Rule (unwords $ init o) (parseContents $ unwords d)

parseContents :: String -> [Content]
parseContents [] = []
parseContents s =
  let (a, b) = span (/= ',') s
      c = dropWhile (`elem` [',', ' ']) b
  in (
    if a == "no other bags"
      then []
      else do
        parseContent a : parseContents c
  )

parseContent :: String -> Content
parseContent a = 
  let (q, c) = span (`elem` "0123456789") a
      qu = read q :: Int
      co = unwords $ init $ words $ tail c
  in Content co qu

getBagsWithin :: Color -> [Rule] -> Int
getBagsWithin [] _ = 0
getBagsWithin _ [] = 0
getBagsWithin c r =
  let ic = getRuleContents $ head $ filter (\rr -> getRuleColor rr == c) r
  in sum $ map (\i -> getContentQuantity i + getContentQuantity i * getBagsWithin (getContentColor i) r) ic

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates b =
  let l = last b
      i = init b
  in if l `elem` i then removeDuplicates i else l : removeDuplicates i