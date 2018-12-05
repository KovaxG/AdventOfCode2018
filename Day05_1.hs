import Common
import Data.Char

type Polimer = String

react :: Polimer -> Polimer
react = concat . sliding 2 r
  where
    r (a:b:[])
      | differentCase a b && sameCharacter a b = []
      | otherwise = [a,b]
      
    differentCase a b = 
      isUpper a && isLower b || isLower a && isUpper b
      
    sameCharacter a b = toUpper a == toUpper b      

loop :: Polimer -> Polimer
loop = let new = react old
       in if new == old 
          then new
          else loop new

calc :: [String] -> Int
calc = length . loop . unlines